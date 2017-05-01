- [What is switch-window](#org635d132)
  - [Installation](#orgfd4285b)
  - [Configure and Usage](#org1eba81f)
  - [Tips](#orgc006993)
    - [I want to select a window with "a-z" instead of "1-9".](#org2027249)
    - [I want to let window to show bigger label.](#org762ca5e)
    - [I want to **hide** window label when window's number < 3](#orgde87a69)
    - [I want to select minibuffer with label "z".](#org092edbe)
    - [Switch-window seem to conflict with Exwm, how to do?](#org57094d3)
    - [I use text terminal, but I want **bigger** label.](#orgfbed0ef)
    - [I want to use image or icon as label.](#org5f3360a)
    - [\`switch-window-shortcut-appearance' can't satisfy my need. how to do?](#org65793c2)
    - [Have any other similar package exist?](#org75163aa)
  - [Changelog](#org58915ca)
    - [1.5.0 - 2017-04-29](#org51ed153)
    - [1.0.0 - 2015-01-14](#orga683b8a)
    - [0.11 - 2013-09-14](#org339806f)
    - [0.10 - 2011-06-19](#org86e0873)
    - [0.9 - 2010-11-11 - emacs22 called, it wants some support](#orga5f02be)
    - [0.8 - 2010-09-13 - 999](#org92d5814)
    - [0.7 - 2010-08-23 - window-dedicated-p](#org1d82e51)
    - [0.6 - 2010-08-12 - **Minibuf-1**](#org119ca9a)
    - [0.5 - 2010-08-08 - Polishing](#orgcd03879)


<a id="org635d132"></a>

# What is switch-window

switch-window is an emacs window switch tool, which offer a **visual** way to choose a window to switch to, delete, split or other operations.

![img](./snapshots/switch-window.png)


<a id="orgfd4285b"></a>

## Installation

1.  Config melpa source, please read: <http://melpa.org/#/getting-started>
2.  M-x package-install RET switch-window RET

Note: User can install switch-window with [El-Get](http://github.com/dimitri/el-get) too.


<a id="org1eba81f"></a>

## Configure and Usage

    (require 'switch-window)
    (global-set-key (kbd "C-x o") 'switch-window)
    (global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
    (global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
    (global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
    (global-set-key (kbd "C-x 0") 'switch-window-then-delete)

When switch-window is enabled, user can use the below five keys:

| key | command description   |
|--- |--------------------- |
| "i" | Move the border up    |
| "k" | Move the border down  |
| "j" | Move the border left  |
| "k" | Move the border right |
| "b" | Balance windows       |

If you want to customize this feature, please see variable: \`switch-window-extra-map'.


<a id="orgc006993"></a>

## Tips


<a id="org2027249"></a>

### I want to select a window with "a-z" instead of "1-9".

    (setq switch-window-shortcut-style 'qwerty)
    (setq switch-window-qwerty-shortcuts
          '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))


<a id="org762ca5e"></a>

### I want to let window to show bigger label.

    (setq switch-window-increase 6) ;Increase or decrease this number.


<a id="orgde87a69"></a>

### I want to **hide** window label when window's number < 3

    (setq switch-window-threshold 2)


<a id="org092edbe"></a>

### I want to select minibuffer with label "z".

    (setq switch-window-minibuffer-shortcut "z")


<a id="org57094d3"></a>

### Switch-window seem to conflict with Exwm, how to do?

By default, switch-window get user's input with the help of function \`read-event', this approach does not work well with [Exwm](https://github.com/ch11ng/exwm) (Emacs X window manager), user should set the below variable and use minibuffer to get input instead:

    (setq switch-window-input-style 'minibuffer)


<a id="orgfbed0ef"></a>

### I use text terminal, but I want **bigger** label.

The only choice is using asciiart, which **draw** a bigger label with **small** ascii char.

    (setq switch-window-shortcut-appearance 'asciiart)

![img](./snapshots/switch-window-3.png)


<a id="org5f3360a"></a>

### I want to use image or icon as label.

1.  Prepare your label images, rename them to: 1.png &#x2026; 9.png, a.png &#x2026; z.png.

    You can use other image types supported by emacs, please see: \`image-types'.
2.  Put all above images to directory: \`switch-window-image-directory'.
3.  Set variable: \`switch-window-shortcut-appearance'

        (setq switch-window-shortcut-appearance 'image)

![img](./snapshots/switch-window-2.png)


<a id="org65793c2"></a>

### \`switch-window-shortcut-appearance' can't satisfy my need. how to do?

All you should do is hacking you own label buffer function, for example: my-switch-window-label-buffer-function, and set the below variable:

    (setq switch-window-label-buffer-function
          'my-switch-window-label-buffer-function)


<a id="org75163aa"></a>

### Have any other similar package exist?

-   [ace-window](https://github.com/abo-abo/ace-window)


<a id="org58915ca"></a>

## Changelog


<a id="org51ed153"></a>

### 1.5.0 - 2017-04-29

-   Implement commands:
    1.  switch-window-then-maximize
    2.  switch-window-then-delete
    3.  switch-window-then-split-below
    4.  switch-window-then-split-right
    5.  switch-window-then-split-horizontally
    6.  switch-window-then-split-vertically
    7.  switch-window-then-swap-buffer
-   Let switch-window work well with Exwm (Emacs X window manager).
-   User can customize switch-window label's appearance.


<a id="orga683b8a"></a>

### 1.0.0 - 2015-01-14

-   Please fixme.


<a id="org339806f"></a>

### 0.11 - 2013-09-14

-   restore point to end-of-buffer for windows where it was the case after switching, fixing an anoying bug.


<a id="org86e0873"></a>

### 0.10 - 2011-06-19

-   implement M-x delete-other-window (thanks developernotes on github)


<a id="orga5f02be"></a>

### 0.9 - 2010-11-11 - emacs22 called, it wants some support

-   implement a propertize based hack to support emacs22


<a id="org92d5814"></a>

### 0.8 - 2010-09-13 - 999

-   Suport more than 9 windows (with a single key to type)
-   Use quail-keyboard-layout to choose single key labels for windows


<a id="org1d82e51"></a>

### 0.7 - 2010-08-23 - window-dedicated-p

-   temporarily unset the window dedicated flag for displaying the numbers, patch from René Kyllingstad <Rene@Kyllingstad.com>
-   fix timeout and RET handling wrt to not changing window selection


<a id="org119ca9a"></a>

### 0.6 - 2010-08-12 - **Minibuf-1**

-   add support for selecting the minibuffer when it's active
-   some try at a better horizontal centering
-   assorted cleanup


<a id="orgcd03879"></a>

### 0.5 - 2010-08-08 - Polishing

-   dim:switch-window-increase is now a maximum value


Converted from switch-window.el by [el2org](https://github.com/tumashu/el2org) .