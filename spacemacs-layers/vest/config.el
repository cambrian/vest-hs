;; Layer configuration, by layer

;; haskell
(setq hindent-reformat-buffer-on-save t)
(add-hook 'haskell-mode-hook
          (lambda ()
            (hindent-mode)
            (company-mode)
            (setq haskell-process-suggest-hoogle-imports t)
            (setq haskell-process-use-presentation-mode t)
            ))

;; auto-completion
;; does this actually do anything??
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)

;; neotree
(setq projectile-switch-project-action 'neotree-projectile-action)
(add-hook 'window-setup-hook #'neotree-project-dir)
