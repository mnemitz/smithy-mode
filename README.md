[![MELPA](https://melpa.org/packages/smithy-mode-badge.svg)](https://melpa.org/#/smithy-mode)

# Smithy mode

Provides syntax highlighting for the [Smithy IDL](https://awslabs.github.io/smithy/)

In the future this may be extended to more functionality, subject to progress on the [Smithy Language Server implementation](https://github.com/awslabs/smithy-language-server)

## Installation
Smithy mode is available through MELPA.

Basic installation can be done as follows:

```
M-x package-refresh-contents<RET>

M-x package-install<RET>smithy-mode<RET>
```

On DOOM Emacs

```emacs-lisp
;;; ~/.doom.d/packages.el

(package! smithy-mode)
```

If you encounter any issues installing with DOOM, you may need to refresh your MELPA recipes.
