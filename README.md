# ~/.emacs.d

My emacs configurations.

## Requirements

* [Emacs](https://www.gnu.org/software/emacs/) 25+

## Installation

Checkout this repository at `~/.emacs.d`:

```bash
$ git clone https://github.com/kou64yama/dot.emacs.d.git ~/.emacs.d
```

## Key bindings

* global-map
  * `C-x g`: Invoke `magit-status`.
  * `C-x M-g`: Invoke `magit-dispatch-popup`.
  * `C-=`: Invoke `er/expand-region`.
  * `C-S-c C-S-c`: Invoke `mc/edit-lines`.
  * `C-<`: Invoke `mc/mark-previous-like-this`.
  * `C->`: Invoke `mc/mark-next-like-this`.
  * `C-c C-<`: Invoke `mc/mark-all-like-this`.
  * `C-c t`: Invoke `tabbar-ruler-move`.
  * `C-x C-j`: Invoke `ssk-mode`.
  * `C-c C-r` or `<f6>`: Invoke `ivy-resume`.
  * `M-x`: Invoke `counsel-M-x`.
  * `C-x C-f`: Invoke `counsel-find-file`.
  * `<f1> f`: Invoke `counsel-describe-function`.
  * `<f1> v`: Invoke `counsel-describe-variable`.
  * `<f1> l`: Invoke `counsel-find-library`.
  * `<f2> i`: Invoke `counsel-info-lookup-symbol`.
  * `<f2> u`: Invoke `counsel-unicode-char`.
  * `C-c g`: Invoke `counsel-git`.
  * `C-c j`: Invoke `counsel-git-grep`.
  * `C-c k`: Invoke `counsel-ag`.
  * `C-x l`: Invoke `counsel-locate`.
  * `C-S-o`: Invoke `counsel-rhythmbox`.
  * `C-s`: Invoke `swiper`.
  * `<M-up>`: Invoke `drag-stuff-up`
  * `<M-down>`: Invoke `drag-stuff-down`
  * `<M-right>`: Invoke `drag-stuff-right`
  * `<M-left>`: Invoke `drag-stuff-left`
* read-expression-map
  * `C-r`: Invoke `counsel-expression-history`.

## Packages

* [adoc-mode](https://github.com/sensorflo/adoc-mode/wiki)
* [company-flow](https://github.com/aaronjensen/company-flow)
* [company-go](https://github.com/nsf/gocode/tree/master/emacs-company)
* [company](https://company-mode.github.io)
* [counsel-projectile](https://github.com/ericdanan/counsel-projectile)
* [counsel](https://github.com/abo-abo/swiper#counsel)
* [ddskk](http://openlab.jp/skk/)
* [drag-stuff](https://github.com/rejeep/drag-stuff.el)
* [editorconfig](https://github.com/editorconfig/editorconfig-emacs)
* [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell)
* [expand-region](https://github.com/magnars/expand-region.el)
* [flycheck-flow](https://github.com/lbolla/emacs-flycheck-flow)
* [flycheck](http://www.flycheck.org/en/latest/)
* [font-utils](https://github.com/rolandwalker/font-utils)
* [git-gutter-fringe+](https://github.com/nonsequitur/git-gutter-fringe-plus)
* [go-mode](https://github.com/dominikh/go-mode.el)
* [google-translate](https://github.com/atykhonov/google-translate)
* [groovy-mode](https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes)
* [ivy](https://github.com/abo-abo/swiper#ivy)
* [js2-mode](https://github.com/mooz/js2-mode)
* [json-mode](https://github.com/joshwnj/json-mode)
* [keyfreq](https://github.com/dacap/keyfreq)
* [magit](https://magit.vc)
* [markdown-mode](http://jblevins.org/projects/markdown-mode/)
* [nord-theme](https://emacsthemes.com/themes/nord-theme.html)
* [mode-icons](http://projects.ryuslash.org/mode-icons/)
* [multiple-cursors](https://github.com/magnars/multiple-cursors.el)
* [projectile](https://projectile.readthedocs.io/en/latest/)
* [rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters)
* [smart-mode-line-powerline-theme](https://github.com/Malabarba/smart-mode-line/)
* [smartparens](https://github.com/Fuco1/smartparens)
* [swiper](https://github.com/abo-abo/swiper#swiper)
* [tide](https://github.com/ananthakumaran/tide)
* [typescript-mode](https://github.com/ananthakumaran/typescript.el)
* [undo-tree](https://www.emacswiki.org/emacs/UndoTree)
* [undohist](https://github.com/m2ym/undohist-el)
* [use-package](https://github.com/jwiegley/use-package)
* [web-mode](http://web-mode.org)
* [yascroll](https://github.com/m2ym/yascroll-el)
* [yasnippet](https://joaotavora.github.io/yasnippet/)

## License

[MIT](https://github.com/kou64yama/dot.emacs.d/blob/master/LICENSE)

## Author

[YAMADA Koji](https://github.com/kou64yama)
