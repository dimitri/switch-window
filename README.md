- [What is switch-window](#org70e3445)
  - [Installation](#org29f569a)
  - [Configure and Usage](#orgf2331b8)
  - [Tips](#org18db2bc)
    - [I want to select a window with "a-z" instead of "1-9".](#org63de3f4)
    - [I want to let window to show bigger label.](#org9f6ff58)
    - [I want to **hide** window label when window's number < 3](#orgf648e90)
    - [I want to select minibuffer with label "z".](#orgf4f0054)
    - [I want to auto resize a window when switch to it](#orgd316a14)
    - [Switch-window seem to conflict with Exwm, how to do?](#orgc55c056)
    - [I use text terminal, but I want **bigger** label.](#org6d8e15f)
    - [I want to use image or icon as label.](#org7426091)
    - [\`switch-window-shortcut-appearance' can't satisfy my need. how to do?](#orgb0b3b45)
    - [Have any other similar package exist?](#orgdc6b655)
  - [Changelog](#org15c75ea)
    - [1.5.0 - 2017-04-29](#orgd87b6c2)
    - [1.0.0 - 2015-01-14](#org9120a2a)
    - [0.11 - 2013-09-14](#orgcfef3eb)
    - [0.10 - 2011-06-19](#orgd74e7fe)
    - [0.9 - 2010-11-11 - emacs22 called, it wants some support](#org37cc114)
    - [0.8 - 2010-09-13 - 999](#orgce69bee)
    - [0.7 - 2010-08-23 - window-dedicated-p](#org868e6ee)
    - [0.6 - 2010-08-12 - **Minibuf-1**](#org3f9689b)
    - [0.5 - 2010-08-08 - Polishing](#orga93a788)


<a id="org70e3445"></a>

# What is switch-window

switch-window is an emacs window switch tool, which offer a **visual** way to choose a window to switch to, delete, split or other operations.

![img](./snapshots/switch-window.png)


<a id="org29f569a"></a>

## Installation

1.  Config melpa source, please read: <http://melpa.org/#/getting-started>
2.  M-x package-install RET switch-window RET

Note: User can install switch-window with [El-Get](http://github.com/dimitri/el-get) too.


<a id="orgf2331b8"></a>

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


<a id="org18db2bc"></a>

## Tips


<a id="org63de3f4"></a>

### I want to select a window with "a-z" instead of "1-9".

    (setq switch-window-shortcut-style 'qwerty)
    (setq switch-window-qwerty-shortcuts
          '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))


<a id="org9f6ff58"></a>

### I want to let window to show bigger label.

    (setq switch-window-increase 6) ;Increase or decrease this number.


<a id="orgf648e90"></a>

### I want to **hide** window label when window's number < 3

    (setq switch-window-threshold 2)


<a id="orgf4f0054"></a>

### I want to select minibuffer with label "z".

    (setq switch-window-minibuffer-shortcut ?z)


<a id="orgd316a14"></a>

### I want to auto resize a window when switch to it

    (setq switch-window-auto-resize-window t)
    (setq switch-window-default-window-size 0.8) ;80% of frame size

Advanced usage:

    (setq switch-window-auto-resize-window
          (lambda ()
            (equal (buffer-name) "*scratch*"))) ;when return t, run auto switch
    (setq switch-window-default-window-size '(0.8 . 0.6)) ;80% width and 60% height of frame

By the way, you can use package [golden-ratio](https://github.com/roman/golden-ratio.el) also.


<a id="orgc55c056"></a>

### Switch-window seem to conflict with Exwm, how to do?

By default, switch-window get user's input with the help of function \`read-event', this approach does not work well with [Exwm](https://github.com/ch11ng/exwm) (Emacs X window manager), user should set the below variable and use minibuffer to get input instead:

    (setq switch-window-input-style 'minibuffer)

Note: if you use minibuffer to get input, the feature about \`switch-window-minibuffer-shortcut' will not work well.


<a id="org6d8e15f"></a>

### I use text terminal, but I want **bigger** label.

The only choice is using asciiart, which **draw** a bigger label with **small** ascii char.

    (setq switch-window-shortcut-appearance 'asciiart)

![img](./snapshots/switch-window-3.png)


<a id="org7426091"></a>

### I want to use image or icon as label.

1.  Prepare your label images, rename them to: 1.png &#x2026; 9.png, a.png &#x2026; z.png.

    You can use other image types supported by emacs, please see: \`image-types'.
2.  Put all above images to directory: \`switch-window-image-directory'.
3.  Set variable: \`switch-window-shortcut-appearance'

        (setq switch-window-shortcut-appearance 'image)

![img](./snapshots/switch-window-2.png)


<a id="orgb0b3b45"></a>

### \`switch-window-shortcut-appearance' can't satisfy my need. how to do?

All you should do is hacking you own label buffer function, for example: my-switch-window-label-buffer-function, and set the below variable:

    (setq switch-window-label-buffer-function
          'my-switch-window-label-buffer-function)


<a id="orgdc6b655"></a>

### Have any other similar package exist?

-   [ace-window](https://github.com/abo-abo/ace-window)


<a id="org15c75ea"></a>

## Changelog


<a id="orgd87b6c2"></a>

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


<a id="org9120a2a"></a>

### 1.0.0 - 2015-01-14

-   Please fixme.


<a id="orgcfef3eb"></a>

### 0.11 - 2013-09-14

-   restore point to end-of-buffer for windows where it was the case after switching, fixing an anoying bug.


<a id="orgd74e7fe"></a>

### 0.10 - 2011-06-19

-   implement M-x delete-other-window (thanks developernotes on github)


<a id="org37cc114"></a>

### 0.9 - 2010-11-11 - emacs22 called, it wants some support

-   implement a propertize based hack to support emacs22


<a id="orgce69bee"></a>

### 0.8 - 2010-09-13 - 999

-   Suport more than 9 windows (with a single key to type)
-   Use quail-keyboard-layout to choose single key labels for windows


<a id="org868e6ee"></a>

### 0.7 - 2010-08-23 - window-dedicated-p

-   temporarily unset the window dedicated flag for displaying the numbers, patch from René Kyllingstad <Rene@Kyllingstad.com>
-   fix timeout and RET handling wrt to not changing window selection


<a id="org3f9689b"></a>

### 0.6 - 2010-08-12 - **Minibuf-1**

-   add support for selecting the minibuffer when it's active
-   some try at a better horizontal centering
-   assorted cleanup


<a id="orga93a788"></a>

### 0.5 - 2010-08-08 - Polishing

-   dim:switch-window-increase is now a maximum value


Converted from switch-window.el by [el2org](https://github.com/tumashu/el2org) .