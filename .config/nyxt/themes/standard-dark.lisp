(in-package #:nyxt-user)

;;configuration window
(define-configuration window
  ((message-buffer-style
    (str:concat
     %slot-default%
     (cl-css:css
      '((body
         :font-family "monospace,monospace"
         :font-size "10px"
         :background-color "#292d3e"
         :color "#bfc7e5")))))))

;prompt buffer
(define-configuration prompt-buffer
  ((style (str:concat
           %slot-default%
           (cl-css:css
            '((*
               :font-family "monospace,monospace"
               :font-size "12px"
               :line-height "12px")
              (body
               :background-color "#292d3e"
               :color "#bfc7e5")
              ("#vi-mode"
               :line-height "18px")
              ("#prompt-area"
               :background-color "#292d3e")
              ("#prompt-area-vi"
               :background-color "#292d3e")
              ("#input"
               :background-color "#3D425A"
               :color "#cccccc")
              (".source-name"
               :background-color "#141724"
               :color "#cccccc")
              (".source-content"
               :width "100%"
               :background-color "#292d3e")
              (".source-content th"
               :background-color "292d3e"
               :font-weight "bold")
              (".source-content th:nth-child(1)"
               :width "20%")
              (".source-content th:nth-child(2)"
               :width "9%")
              (".source-content th:nth-child(3)"
               :width "54%")
              (".source-content th:nth-child(4)"
               :width "17%")
              ("#selection"
               :background-color "#141724")
              (.marked :background-color "black"
                       :font-weight "bold"
                       :color "#cccccc")
              (.selected :background-color "black"
                         :color "#cccccc")))))))

;internal buffer
(define-configuration internal-buffer
  ((style
    (str:concat
     %slot-default%
     (cl-css:css
      '((title
         :color "#bfc7e5")
        (body
         :background-color "#292d3e"
         :color "#bfc7e5")
        (hr
         :color "#bfc7e5")
        (a
         :color "#bfc7e5")
        (.button
         :color "#292d3e"
         :background-color "#bfc7e5")))))))

;;history tree
(define-configuration nyxt/history-tree-mode:history-tree-mode
  ((nyxt/history-tree-mode::style
    (str:concat
     %slot-default%
     (cl-css:css
      '((body
         :background-color "#292d3e"
         :color "#bfc7e5")
        (hr
         :color "#3d425a")
        (a
         :color "#bfc7e5")
        ("ul li::before"
         :background-color "#bfc7e5")
        ("ul li::after"
         :background-color "#bfc7e5")
        ("ul li:only-child::before"
         :background-color "#bfc7e5")))))))

;;highlight boxes
(define-configuration nyxt/web-mode:web-mode
  ((nyxt/web-mode:highlighted-box-style
    (cl-css:css
     '((".nyxt-hint.nyxt-highlight-hint"
        :background "#292d3e")))
    :documentation "The style of highlighted boxes, e.g. link hints.")))

;;styling status buffer black and setting size of tabs and modes. Can uncomment url and controls section for theming those.
(define-configuration status-buffer
  ((style (str:concat
           %slot-default%
           (cl-css:css
            '((*
               :font-family "monospace,monospace")
              ("#controls"
               :background-color "#292d3e"
               :border-top "1px solid black")
              ("#url"
               :background-color "#292d3e"
               :border-top "1px solid black"
               :font-size "11px")
              ("#modes"
               :background-color "#292d3e"
               :border-top "1px solid black"
               :font-size "11px")
              ("#tabs"
               :background-color "#292d3e"
               :color "#bfc7e5"
               :font-size "11px"
               :border-top "1px solid black")
              (".controls:hover"
               :color "#bfc7e5")
              (".modes:hover"
               :color "#bfc7e5")
              (".button:hover"
               :color "#bfc7e5")
              (".url:hover"
               :color "#bfc7e5")
              (".tab:hover"
               :color "#bfc7e5")))))))
