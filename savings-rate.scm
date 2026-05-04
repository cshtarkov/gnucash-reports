;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; savings-rate.scm: Savings rate from Profit & Loss
;;
;; Copyright (C) 2026  Christian Shtarkov
;;
;; Based on income-statement.scm by:
;; David Montenegro <sunrise2000@comcast.net>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report savings-rate))
(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))

(define report-name (N_ "Savings Rate"))

(define optname-report-title (N_ "Report Title"))
(define opthelp-report-title (N_ "Title for this report."))

(define optname-start-date (N_ "Start Date"))
(define optname-end-date (N_ "End Date"))

(define optname-accounts (N_ "Income and Expense Accounts"))
(define opthelp-accounts
  (N_ "Report on these income and expense accounts."))
(define optname-tax-accounts (N_ "Tax Accounts"))
(define opthelp-tax-accounts
  (N_ "Report on these tax accounts. Should not overlap with the expense accounts."))

(define pagename-commodities (N_ "Commodities"))
(define optname-report-commodity (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-show-rates (N_ "Show Exchange Rates"))
(define opthelp-show-rates (N_ "Show the exchange rates used."))

(define pagename-entries (N_ "Entries"))
(define optname-closing-pattern (N_ "Closing Entries pattern"))
(define opthelp-closing-pattern
  (N_ "Any text in the Description column which identifies closing entries."))
(define optname-closing-casing
  (N_ "Closing Entries pattern is case-sensitive"))
(define opthelp-closing-casing
  (N_ "Causes the Closing Entries Pattern match to be case-sensitive."))
(define optname-closing-regexp
  (N_ "Closing Entries Pattern is regular expression"))
(define opthelp-closing-regexp
  (N_ "Causes the Closing Entries Pattern to be treated as a regular expression."))

(define (savings-rate-options-generator)
  (let* ((options (gnc-new-optiondb)))

    (gnc-register-string-option options
                                gnc:pagename-general optname-report-title
                                "a" opthelp-report-title (G_ report-name))

    ;; period over which to report income
    (gnc:options-add-date-interval!
     options gnc:pagename-general
     optname-start-date optname-end-date "c")

    ;; accounts to work on
    (gnc-register-account-list-option
     options
     gnc:pagename-accounts optname-accounts
     "a"
     opthelp-accounts
     (gnc:filter-accountlist-type
      ;; select, by default, only income and expense accounts
      (list ACCT-TYPE-INCOME ACCT-TYPE-EXPENSE)
      (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))

    ;; accounts to treat as tax accounts
    (gnc-register-account-list-limited-option
     options
     gnc:pagename-accounts optname-tax-accounts
     "b"
     opthelp-tax-accounts
     '()
     (list ACCT-TYPE-EXPENSE))

    ;; all about currencies
    (gnc:options-add-currency!
     options pagename-commodities
     optname-report-commodity "a")

    (gnc:options-add-price-source!
     options pagename-commodities
     optname-price-source "b" 'pricedb-nearest)

    (gnc-register-simple-boolean-option options
                                        pagename-commodities optname-show-rates
                                        "d" opthelp-show-rates #f)
    
    ;; closing entry match criteria
    ;;
    ;; N.B.: transactions really should have a field where we can put
    ;; transaction types like "Adjusting/Closing/Correcting Entries"
    (gnc-register-string-option options
                                pagename-entries optname-closing-pattern
                                "a" opthelp-closing-pattern (G_ "Closing Entries"))
    (gnc-register-simple-boolean-option options
                                        pagename-entries optname-closing-casing
                                        "b" opthelp-closing-casing #f)
    (gnc-register-simple-boolean-option options
                                        pagename-entries optname-closing-regexp
                                        "c" opthelp-closing-regexp #f)

    ;; Set the accounts page as default option tab
    (gnc:options-set-default-section options gnc:pagename-accounts)

    options))

(define (savings-rate-renderer report-obj)
  (define (get-option pagename optname)
    (gnc-optiondb-lookup-value
     (gnc:report-options report-obj) pagename optname))

  (gnc:report-starting report-name)

  ;; get all option's values
  (let* (
         (report-title (get-option gnc:pagename-general optname-report-title))
         (company-name (or (gnc:company-info (gnc-get-current-book) gnc:*company-name*) ""))
         (start-date-printable (gnc:date-option-absolute-time
                                (get-option gnc:pagename-general
                                            optname-start-date)))
         (start-date (gnc:time64-start-day-time
                      (gnc:date-option-absolute-time
                       (get-option gnc:pagename-general
                                   optname-start-date))))
         (end-date (gnc:time64-end-day-time
                    (gnc:date-option-absolute-time
                     (get-option gnc:pagename-general
                                 optname-end-date))))
         (accounts (get-option gnc:pagename-accounts
                               optname-accounts))
         (tax-accounts (get-option gnc:pagename-accounts
                                   optname-tax-accounts))
         (report-commodity (get-option pagename-commodities
                                       optname-report-commodity))
         (price-source (get-option pagename-commodities
                                   optname-price-source))
         (show-rates? (get-option pagename-commodities
                                  optname-show-rates))
         (closing-str (get-option pagename-entries
                                  optname-closing-pattern))
         (closing-cased (get-option pagename-entries
                                    optname-closing-casing))
         (closing-regexp (get-option pagename-entries
                                     optname-closing-regexp))
         (closing-pattern
          (list (list 'str closing-str)
                (list 'cased closing-cased)
                (list 'regexp closing-regexp)
                (list 'closing #t)))

         ;; decompose the account list
         (split-up-accounts (gnc:decompose-accountlist accounts))
         (revenue-accounts (assoc-ref split-up-accounts ACCT-TYPE-INCOME))
         (trading-accounts (assoc-ref split-up-accounts ACCT-TYPE-TRADING))
         (expense-accounts (assoc-ref split-up-accounts ACCT-TYPE-EXPENSE))

         (doc (gnc:make-html-document))
         
         ;; exchange rates calculation parameters
         (exchange-fn
          (gnc:case-exchange-fn price-source report-commodity end-date))
         (price-fn (gnc:case-price-fn price-source report-commodity end-date)))

    (define (collector/ a b)
      (let ((a-value (gnc:gnc-monetary-amount
                      (gnc:sum-collector-commodity a report-commodity exchange-fn)))
            (b-value (gnc:gnc-monetary-amount
                      (gnc:sum-collector-commodity b report-commodity exchange-fn))))
        (if (= 0 b-value) 0 (/ a-value b-value))))

    (define (calculate-savings-rate-percentage net-income income)
      (let ((res (* 100 (collector/ net-income income))))
        (max 0 res)))

    (define (add-rule table) (gnc:html-table-append-ruler! table 2))

    (gnc:html-document-set-title!
     doc (gnc:format
          (G_ "${company-name} ${report-title} For Period Covering ${start} to ${end}")
          'company-name company-name
          'report-title report-title
          'start (qof-print-date start-date-printable)
          'end (qof-print-date end-date)))

    (if (null? accounts)

        (gnc:html-document-add-object!
         doc (gnc:html-make-no-account-warning
              report-name (gnc:report-id report-obj)))

        ;; Get all the balances for each of the account types.
        (let* ((expense-total
                (gnc:collector-
                 (gnc:accountlist-get-comm-balance-interval-with-closing
                  expense-accounts start-date end-date)
                 (gnc:account-get-trans-type-balance-interval-with-closing
                  expense-accounts closing-pattern start-date end-date)))

               (tax-total
                (gnc:collector-
                 (gnc:accountlist-get-comm-balance-interval-with-closing
                  tax-accounts start-date end-date)
                 (gnc:account-get-trans-type-balance-interval-with-closing
                  tax-accounts closing-pattern start-date end-date)))

               (revenue-total
                (gnc:collector-
                 (gnc:account-get-trans-type-balance-interval-with-closing
                  revenue-accounts closing-pattern start-date end-date)
                 (gnc:accountlist-get-comm-balance-interval-with-closing
                  revenue-accounts start-date end-date)))

               (trading-total
                (gnc:accountlist-get-comm-balance-interval-with-closing
                 trading-accounts start-date end-date))

               (revenue-and-trading-total
                (gnc:collector+ revenue-total trading-total))

               (revenue-and-trading-total-net-of-tax
                (gnc:collector- revenue-and-trading-total tax-total))

               (net-income
                (gnc:collector- revenue-and-trading-total tax-total expense-total))

               (savings-rate
                (calculate-savings-rate-percentage
                 net-income
                 revenue-and-trading-total))

               (savings-rate-net-of-tax
                (calculate-savings-rate-percentage
                 net-income
                 revenue-and-trading-total-net-of-tax))

               (build-table (gnc:make-html-table))

               (period-for (string-append " " (G_ "for Period"))))

          (define (add-header table label)
            (gnc:html-table-add-labeled-amount-line!
             table 0 "primary-subheading" #f
             label 0 1 "text-cell"
             #f    0 1 #f))

          (define (add-report-line-value table row-markup label value)
            (gnc:html-table-add-labeled-amount-line!
             table 0 row-markup #f
             label 0 1 "text-cell"
             value 0 1 "number-cell"))

          (define (add-report-line-amount table label amount)
            (let ((value (gnc:sum-collector-commodity
                          amount report-commodity exchange-fn)))
              (add-report-line-value table #f label value)))

          (define (add-report-line-no-value table label)
            (gnc:html-table-add-labeled-amount-line!
             table 0 #f #f
             label 0 1 "text-cell"
             "-"   0 1 "number-cell"))

          (define (add-savings-rate table label value)
            (add-report-line-value
             table
             "primary-subheading"
             label
             (format #f "~,2f%" value)))

          (let ((gross-table (gnc:make-html-table))
                (net-table   (gnc:make-html-table)))

            (add-header gross-table (G_ "Gross of Tax"))
            (add-report-line-amount
             gross-table
             (G_ "Income")
             revenue-and-trading-total)
            (add-report-line-amount
             gross-table
             (G_ "Tax")
             tax-total)
            (add-report-line-amount
             gross-table
             (G_ "Expenses")
             expense-total)
            (add-rule gross-table)
            (add-report-line-amount
             gross-table
             (string-append (G_ "Difference") period-for)
             net-income)
            (add-savings-rate
             gross-table
             (string-append (G_ "Savings Rate") period-for)
             savings-rate)

            (add-header net-table (G_ "Net of Tax"))
            (add-report-line-amount
             net-table
             (G_ "Income")
             revenue-and-trading-total-net-of-tax)
            (add-report-line-no-value
             net-table
             (G_ "Tax"))
            (add-report-line-amount
             net-table
             (G_ "Expenses")
             expense-total)
            (add-rule net-table)
            (add-report-line-amount
             net-table
             (string-append (G_ "Difference") period-for)
             net-income)
            (add-savings-rate
             net-table
             (string-append (G_ "Savings Rate") period-for)
             savings-rate-net-of-tax)

            (gnc:html-table-append-row! build-table (list gross-table net-table)))

          (gnc:html-table-set-style!
           build-table "td"
           'attribute '("align" "left")
           'attribute '("valign" "top"))

          (gnc:html-document-add-object! doc build-table)

          ;; add currency information if requested
          (gnc:report-percent-done 90)
          (when show-rates?
            (gnc:html-document-add-object!
             doc (gnc:html-make-rates-table
                  report-commodity price-fn accounts)))
          (gnc:report-percent-done 100)))

    (gnc:report-finished)

    doc))

(gnc:define-report
 'version 1
 'name (N_ "Savings Rate")
 'report-guid "4563b3a4ba2a4c379da065d6133fa17f"
 'menu-tip (N_ "Calculate a savings rate from P&L.")
 'menu-path (list gnc:menuname-income-expense)
 'options-generator savings-rate-options-generator
 'renderer savings-rate-renderer)

;; END

;; Local Variables:
;; fill-column: 92
;; End:
