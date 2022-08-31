(in-package #:nyxt-user)

;; Set StatusLines Mode Icons
(define-configuration status-buffer ((glyph-mode-presentation-p t)))
(define-configuration nyxt/force-https-mode:force-https-mode ((glyph "🔒")))
(define-configuration nyxt/blocker-mode:blocker-mode ((glyph "🌭")))
(define-configuration nyxt/proxy-mode:proxy-mode ((glyph "p")))
(define-configuration nyxt/user-script-mode:user-script-mode ((glyph "📃")))
(define-configuration nyxt/reduce-tracking-mode:reduce-tracking-mode  ((glyph "☠")))
(define-configuration nyxt/certificate-exception-mode:certificate-exception-mode ((glyph "ssl")))
(define-configuration nyxt/style-mode:style-mode ((glyph "s")))
(define-configuration nyxt/help-mode:help-mode ((glyph "H")))
(define-configuration nyxt/web-mode:web-mode ((glyph "ω")))
(define-configuration nyxt/auto-mode:auto-mode ((glyph "✈")))
