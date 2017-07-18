- [What is switch-window](#org4330b75)
  - [Installation](#org39217cc)
  - [Configure and Usage](#orgcf83c72)
  - [Tips](#org2be66f4)
    - [I want to select a window with "a-z" instead of "1-9".](#orgb2be0ed)
    - [I want to let window to show bigger label.](#orgc610fdf)
    - [I want to **hide** window label when window's number < 3](#org2cc8b5c)
    - [I want to select minibuffer with label "z".](#orge9f2861)
    - [I want to auto resize a window when switch to it](#orgadf72bb)
    - [Switch-window seem to conflict with Exwm, how to do?](#org265156a)
    - [I use text terminal, but I want **bigger** label.](#org508e1b3)
    - [I want to use image or icon as label.](#org2a544eb)
    - [\`switch-window-shortcut-appearance' can't satisfy my need. how to do?](#org702dbef)
    - [Have any other similar package exist?](#orgfbfa91a)
  - [Changelog](#orga3ded3f)
    - [1.5.0 - 2017-04-29](#org8ef0c41)
    - [1.0.0 - 2015-01-14](#org9cafb5e)
    - [0.11 - 2013-09-14](#org2328693)
    - [0.10 - 2011-06-19](#org02f0b29)
    - [0.9 - 2010-11-11 - emacs22 called, it wants some support](#org74a630b)
    - [0.8 - 2010-09-13 - 999](#org8f869e1)
    - [0.7 - 2010-08-23 - window-dedicated-p](#org7431bac)
    - [0.6 - 2010-08-12 - **Minibuf-1**](#orgac57154)
    - [0.5 - 2010-08-08 - Polishing](#org4bb2fb3)


<a id="org4330b75"></a>

# What is switch-window

switch-window is an emacs window switch tool, which offer a **visual** way to choose a window to switch to, delete, split or other operations.

![img](./snapshots/switch-window.png)


<a id="org39217cc"></a>

## Installation

1.  Config melpa source, please read: <http://melpa.org/#/getting-started>
2.  M-x package-install RET switch-window RET

Note: User can install switch-window with [El-Get](http://github.com/dimitri/el-get) too.


<a id="orgcf83c72"></a>

## Configure and Usage

    (require 'switch-window)
    (global-set-key (kbd "C-x o") 'switch-window)
    (global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
    (global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
    (global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
    (global-set-key (kbd "C-x 0") 'switch-window-then-delete)

When switch-window is enabled, user can use the below five keys:

| key   | command description   |
|----- |--------------------- |
| "i"   | Move the border up    |
| "k"   | Move the border down  |
| "j"   | Move the border left  |
| "k"   | Move the border right |
| "b"   | Balance windows       |
| "SPC" | Resume auto-resize    |

If you want to customize this feature, please see variable: \`switch-window-extra-map'.

Note: if you use auto-resize window feature, you **must** know that when you execute above window operate commands, auto-resize feature will be disabled temporarily, you should use above "SPC" key to resume.


<a id="org2be66f4"></a>

## Tips


<a id="orgb2be0ed"></a>

### I want to select a window with "a-z" instead of "1-9".

    (setq switch-window-shortcut-style 'qwerty)
    (setq switch-window-qwerty-shortcuts
          '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))


<a id="orgc610fdf"></a>

### I want to let window to show bigger label.

    (setq switch-window-increase 6) ;Increase or decrease this number.


<a id="org2cc8b5c"></a>

### I want to **hide** window label when window's number < 3

    (setq switch-window-threshold 2)


<a id="orge9f2861"></a>

### I want to select minibuffer with label "z".

    (setq switch-window-minibuffer-shortcut ?z)


<a id="orgadf72bb"></a>

### I want to auto resize a window when switch to it

    (setq switch-window-auto-resize-window t)
    (setq switch-window-default-window-size 0.8) ;80% of frame size

Advanced usage:

    (setq switch-window-auto-resize-window
          (lambda ()
            (equal (buffer-name) "*scratch*"))) ;when return t, run auto switch
    (setq switch-window-default-window-size '(0.8 . 0.6)) ;80% width and 60% height of frame

By the way, you can use package [golden-ratio](https://github.com/roman/golden-ratio.el) also.


<a id="org265156a"></a>

### Switch-window seem to conflict with Exwm, how to do?

By default, switch-window get user's input with the help of function \`read-event', this approach does not work well with [Exwm](https://github.com/ch11ng/exwm) (Emacs X window manager), user should set the below variable and use minibuffer to get input instead:

    (setq switch-window-input-style 'minibuffer)

Note: if you use minibuffer to get input, the feature about \`switch-window-minibuffer-shortcut' will not work well.


<a id="org508e1b3"></a>

### I use text terminal, but I want **bigger** label.

The only choice is using asciiart, which **draw** a bigger label with **small** ascii char.

    (setq switch-window-shortcut-appearance 'asciiart)

![img](./snapshots/switch-window-3.png)


<a id="org2a544eb"></a>

### I want to use image or icon as label.

1.  Prepare your label images, rename them to: 1.png &#x2026; 9.png, a.png &#x2026; z.png.

    You can use other image types supported by emacs, please see: \`image-types'.
2.  Put all above images to directory: \`switch-window-image-directory'.
3.  Set variable: \`switch-window-shortcut-appearance'

        (setq switch-window-shortcut-appearance 'image)

![img](./snapshots/switch-window-2.png)


<a id="org702dbef"></a>

### \`switch-window-shortcut-appearance' can't satisfy my need. how to do?

All you should do is hacking you own label buffer function, for example: my-switch-window-label-buffer-function, and set the below variable:

    (setq switch-window-label-buffer-function
          'my-switch-window-label-buffer-function)


<a id="orgfbfa91a"></a>

### Have any other similar package exist?

-   [ace-window](https://github.com/abo-abo/ace-window)


<a id="orga3ded3f"></a>

## Changelog


<a id="org8ef0c41"></a>

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


<a id="org9cafb5e"></a>

### 1.0.0 - 2015-01-14

-   Please fixme.


<a id="org2328693"></a>

### 0.11 - 2013-09-14

-   restore point to end-of-buffer for windows where it was the case after switching, fixing an anoying bug.


<a id="org02f0b29"></a>

### 0.10 - 2011-06-19

-   implement M-x delete-other-window (thanks developernotes on github)


<a id="org74a630b"></a>

### 0.9 - 2010-11-11 - emacs22 called, it wants some support

-   implement a propertize based hack to support emacs22


<a id="org8f869e1"></a>

### 0.8 - 2010-09-13 - 999

-   Suport more than 9 windows (with a single key to type)
-   Use quail-keyboard-layout to choose single key labels for windows


<a id="org7431bac"></a>

### 0.7 - 2010-08-23 - window-dedicated-p

-   temporarily unset the window dedicated flag for displaying the numbers, patch from René Kyllingstad <Rene@Kyllingstad.com>
-   fix timeout and RET handling wrt to not changing window selection


<a id="orgac57154"></a>

### 0.6 - 2010-08-12 - **Minibuf-1**

-   add support for selecting the minibuffer when it's active
-   some try at a better horizontal centering
-   assorted cleanup


<a id="org4bb2fb3"></a>

### 0.5 - 2010-08-08 - Polishing

-   dim:switch-window-increase is now a maximum value


Converted from switch-window.el by [el2org](https://github.com/tumashu/el2org) .