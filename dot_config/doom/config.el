;; (general-auto-unbind-keys :off)
;; (remove-hook 'doom-after-init-modules-hook #'general-auto-unbind-keys)

(setq user-full-name "Patrick Wulfe"
      user-mail-address "wulfep@gmail.com")

(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 16)
      doom-unicode-font (font-spec :family "FiraCode Nerd Font")
      doom-variable-pitch-font (font-spec :family "Open Sans" :size 13 :weight 'medium)
      doom-big-font (font-spec :family "Open Sans")
      doom-serif-font (font-spec :family "FiraCode Nerd Font"))

(setq doom-theme 'my-doom-horizon)
(setq doom-modeline-major-mode-icon t)

(defface font-lock-operator-face
  '((t (:foreground "#21BFC2"))) "Basic face for operator." :group 'basic-faces)
;; C-Like
(dolist (mode-iter '(c-mode c++-mode dart-mode glsl-mode java-mode javascript-mode rust-mode))
  (font-lock-add-keywords mode-iter
   '(("\\([~^&\|!<>=,.\\+*/%-;:?]\\)" 0 'font-lock-operator-face keep))))
;; Scripting
(dolist (mode-iter '(python-mode lua-mode))
  (font-lock-add-keywords mode-iter
  '(("\\([@~^&\|!<>:=,.\\+*/%-]\\)" 0 'font-lock-operator-face keep))))

(set-frame-parameter (selected-frame) 'alpha-background 97)
(add-to-list 'default-frame-alist '(alpha-background . 97))

(use-package! nyan-mode
  :after doom-modeline
  :init
  (setq nyan-bar-length 40)
  (setq nyan-animate-cat t)
  (setq nyan-wavy-trail t)
   (nyan-mode))
;; Start animation on page load
(add-hook 'text-mode-hook 'nyan-start-animation)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(setq display-line-numbers-type 'relative)

(use-package! highlight-indent-guides
  :config
  (setq highlight-indent-guides-responsive 'stack)
  ;; (setq highlight-indent-guides-auto-odd-face-perc 30)
  ;; (setq highlight-indent-guides-auto-even-face-perc 20)
  ;; (setq highlight-indent-guides-auto-character-face-perc 30)
  )

(use-package! zoom
  :config
  (setq zoom-mode t)
  (setq zoom-ignore-predicates (lambda ()
    (equal which-key-buffer-name
         (buffer-file-name (current-buffer))))))

(good-scroll-mode 1)
(scroll-bar-mode -1)

(setq confirm-kill-emacs nil)

(setq undo-limit 80000000
      evil-want-fine-undo t)

(setq org-directory "~/org/")

(map! :leader
        (:prefix ("f ." . "open dotfile")
         :desc "Edit doom config.org" "d" #'(lambda () (interactive) (find-file "~/.config/doom/config.org"))
         :desc "Open qtile README.org" "q" #'(lambda () (interactive) (find-file "~/.config/qtile/README.org"))
         :desc "Edit alacritty.yml" "a" #'(lambda () (interactive) (find-file "~/.config/alacritty/alacritty.yml"))
         :desc "Open fish README.org" "f" #'(lambda () (interactive) (find-file "~/.config/fish/README.org"))
         ))

(map! (:after evil-easymotion :leader "j" evilem-map))
(map! :leader :prefix ("j" . "jump"))

(map! :after dart-mode
      :map dart-mode-map
      :localleader
      "l" #'lsp
      "O" #'lsp-dart-show-flutter-outline
      "Q" #'flutter-quit
      "r" #'flutter-hot-reload
      "R" #'flutter-run
      (:prefix ("c" . "create")
                        "b" #'create-dart-bloc
                        "c" #'create-dart-cubit)
      (:prefix ("d" . "debug")
                        "d" #'dap-debug
                        "r" #'lsp-dart-dap-flutter-hot-reload
                        "R" #'lsp-dart-dap-flutter-hot-restart
                        )
      (:prefix ("o" . "open")
                        "d" #'lsp-dart-open-devtools
                        )
      (:prefix ("p" . "pub")
                        "g" #'lsp-dart-pub-get
                        "o" #'lsp-dart-pub-outdated
                        "u" #'lsp-dart-pub-upgrade
                        )
      (:prefix ("t" . "test"))
      )

(map! :leader
      :desc "M-x" "SPC" #'execute-extended-command
      :desc "Find file in project" ":" #'projectile-find-file)

(setq doom-leader-key "SPC"
      doom-localleader-key ",")

(map! :leader
      :desc "Comment operator" ";" #'evilnc-comment-operator)

(map! (:after evil
 :m "C-u" #'good-scroll-down
 :m "C-d" #'good-scroll-up
 :m "C-b" #'good-scroll-down-full-screen
 :m "C-f" #'good-scroll-up-full-screen))

(after! org (map! :localleader
                  :map org-mode-map
                  :desc "Org babel tangle" "B" #'org-babel-tangle))

(map! :leader
      :desc "Go to test/implimentation file" "p j"
      #'projectile-toggle-between-implementation-and-test)

(map! :leader
 (:prefix ("T" . "tree-sitter")
  :desc "TS node at point" "n" #'tree-sitter-node-at-point))

(map! :leader
      (:prefix ("y" . "snippets")
       :desc "Insert" "i" #'yas-insert-snippet
       :desc "New" "n" #'yas-new-snippet
       :desc "Tryout" "t" #'yas-tryout-snippet
      ))

(use-package! company
  :config
  (require 'doom-snippets)
  :after '
    (setq +lsp-company-backends '(:separate company-yasnippet company-capf))
  )

(use-package! lsp-mode
  :custom
  (lsp-headerline-breadcrumb-enable t))

(setq projectile-project-search-path '("~/dev/"))

(setq projectile-create-missing-test-files t)

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (tree-sitter-require 'dart)
  (global-tree-sitter-mode)
  (setq tree-sitter-hl-mode t)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(pushnew! tree-sitter-major-mode-language-alist
          '(dart-mode . dart))

(use-package! treemacs
  :config
  (require 'treemacs-diagnostics)
    (setq doom-themes-treemacs-theme "doom-colors")
    (setq treemacs-position 'right)
  )
;; (setq treemacs-width 30)

(use-package doom-snippets
  :load-path "~/.config/doom/snippets"
  :after yasnippet)

(auto-insert-mode t)
(use-package! yatemplate
  :after yasnippet
  :config
  ;; Define template directory
  (setq yatemplate-dir (concat doom-private-dir "templates"))
  ;; Coupling with auto-insert
  ;; (setq auto-insert-alist nil)
  (yatemplate-fill-alist)
  (add-hook 'find-file-hook 'auto-insert)
  )

(setq evil-snipe-scope 'visible
      evil-snipe-spillover-scope 'buffer)

(use-package! evil-motion-trainer
  :init
  (global-evil-motion-trainer-mode 1)
  :config
  (setq evil-motion-trainer-threshold 5))
;; (setq evil-motion-trainer-super-annoying-mode t)

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(use-package! dart-mode
  :config
  ;; (set-face-foreground 'fill-column-indicator "violet")
  (require 'whitespace)
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face lines-tail)))

(use-package! hover
  :config
  (setq hover-hot-reload-on-save t
        hover-clear-buffer-on-hot-restart t
        hover-screenshot-path "$HOME/Pictures"))

(use-package! lsp-dart
  :after dart-mode
  :config
  (setq lsp-dart-dap-flutter-hot-reload-on-save t))

;; enable lsp on load
(add-hook 'dart-mode-hook 'whitespace-mode)
(add-hook 'dart-mode-hook 'lsp)
(add-hook 'dart-mode-hook #'display-fill-column-indicator-mode)

;; adjust garbage collection
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

(setq lsp-dart-main-code-lens nil
      lsp-dart-test-code-lens nil)

;; (use-package flutter-l10n-flycheck
;;   :after flutter
;;   :config
;;   (flutter-l10n-flycheck-setup))

(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))
(after! projectile
(projectile-register-project-type 'flutter '("pubspec.yaml")
                                  :project-file "pubspec.yaml"
                                  :src-dir "lib/"
                                  :test "flutter test"
                                  :test-dir "test/"
                                  :test-suffix "_test"))

(require 'dap-node)

(setq prettier-js-args '(
                         "--trailing-comma" "all"
                         "--single-quote" "true"
                         "--jsx-single-quote" "true"
                         "--jsx-bracket-same-line" "false"
                         ))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda () (require 'lsp-pyright)
                         (lsp))))

(defun create-dart-bloc (blocSubject)
  "Create dart bloc files (bloc, state, event)"
  (interactive "sBloc Subject: \n")
  (make-empty-file (concat "./bloc/" blocSubject "_bloc.dart"))
  (make-empty-file (concat "./bloc/" blocSubject "_event.dart"))
  (make-empty-file (concat "./bloc/" blocSubject "_state.dart"))
  )
(defun create-dart-cubit (cubitSubject)
  "Create dart cubit files (cubit, state)"
  (interactive "sCubit Subject: \n")
  (make-empty-file (concat "./cubit/" cubitSubject "_cubit.dart"))
  (make-empty-file (concat "./cubit/" cubitSubject "_state.dart"))
  )
