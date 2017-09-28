# Emacs configurations

My emacs configurations.

## Requirements

This configurations requires the following to run:

* [Emacs](https://www.gnu.org/software/emacs/) 25 or newer

## Installation

Checkout this repository at `~/.emacs.d`:

```sh
$ git clone https://github.com/kou64yama/dot.emacs.d.git ~/.emacs.d
```

## Usage

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

* [all-the-icons-dired](https://github.com/jtbm37/all-the-icons-dired)
* [company-flow](https://github.com/aaronjensen/company-flow)
* [company-go](https://github.com/nsf/gocode/tree/master/emacs-company)
* [company](https://company-mode.github.io)
* [counsel-projectile](https://github.com/ericdanan/counsel-projectile)
* [counsel](https://github.com/abo-abo/swiper#counsel)
* [ddskk](http://openlab.jp/skk/)
* [drag-stuff](https://github.com/rejeep/drag-stuff.el)
* [editorconfig](https://github.com/editorconfig/editorconfig-emacs)
* [emojify](https://github.com/iqbalansari/emacs-emojify)
* [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell)
* [expand-region](https://github.com/magnars/expand-region.el)
* [flow-minor-mode](https://github.com/an-sh/flow-minor-mode)
* [flycheck-flow](https://github.com/lbolla/emacs-flycheck-flow)
* [flycheck](http://www.flycheck.org/en/latest/)
* [git-gutter-fringe+](https://github.com/nonsequitur/git-gutter-fringe-plus)
* [go-mode](https://github.com/dominikh/go-mode.el)
* [ivy](https://github.com/abo-abo/swiper#ivy)
* [js2-mode](https://github.com/mooz/js2-mode)
* [json-mode](https://github.com/joshwnj/json-mode)
* [keyfreq](https://github.com/dacap/keyfreq)
* [magit](https://magit.vc)
* [markdown-mode](http://jblevins.org/projects/markdown-mode/)
* [material-theme](https://emacsthemes.com/themes/material-theme.html)
* [mode-icons](http://projects.ryuslash.org/mode-icons/)
* [multiple-cursors](https://github.com/magnars/multiple-cursors.el)
* [projectile](https://projectile.readthedocs.io/en/latest/)
* [rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters)
* [rainbow-mode](https://github.com/emacsmirror/rainbow-mode)
* [smart-mode-line-powerline-theme](https://github.com/Malabarba/smart-mode-line/)
* [smartparens](https://github.com/Fuco1/smartparens)
* [swiper](https://github.com/abo-abo/swiper#swiper)
* [tabbar-ruler](https://github.com/mattfidler/tabbar-ruler.el)
* [tide](https://github.com/ananthakumaran/tide)
* [typescript-mode](https://github.com/ananthakumaran/typescript.el)
* [undo-tree](https://www.emacswiki.org/emacs/UndoTree)
* [undohist](https://github.com/m2ym/undohist-el)
* [use-package](https://github.com/jwiegley/use-package)
* [volatile-highlights](https://www.emacswiki.org/emacs/VolatileHighlights)
* [web-mode](http://web-mode.org)
* [yascroll](https://github.com/m2ym/yascroll-el)
* [yasnippet](https://joaotavora.github.io/yasnippet/)

## License

[MIT](https://github.com/kou64yama/dot.emacs.d/blob/master/LICENSE)

## Author

[YAMADA Koji](https://github.com/kou64yama)
