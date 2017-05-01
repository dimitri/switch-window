- [What is switch-window](#org4aa9944)
  - [Installation](#orgcc2e628)
  - [Configure and Usage](#org1c89b8d)
  - [Tips](#org25d4be3)
    - [I want to select a window with "a-z" instead of "1-9".](#org7393857)
    - [I want to let window to show bigger label.](#org4c3ee94)
    - [I want to **hide** window label when window's number < 3](#orgf74e66a)
    - [I want to select minibuffer with label "z".](#orga354f88)
    - [Switch-window seem to conflict with Exwm, how to do?](#org80dee4a)
    - [I use text terminal, but I want **bigger** label.](#org31494d3)
    - [I want to use image or icon as label.](#orge82fd4d)
    - [\`switch-window-shortcut-appearance' can't satisfy my need. how to do?](#org7d280af)
    - [Have any other similar package exist?](#org24c7efa)
  - [Changelog](#org7bea61e)
    - [1.5.0 - 2017-04-29](#orgf2a1ccd)
    - [1.0.0 - 2015-01-14](#org2049cc7)
    - [0.11 - 2013-09-14](#org27b1972)
    - [0.10 - 2011-06-19](#org314849e)
    - [0.9 - 2010-11-11 - emacs22 called, it wants some support](#org03d1713)
    - [0.8 - 2010-09-13 - 999](#orgd73ee54)
    - [0.7 - 2010-08-23 - window-dedicated-p](#orga77bf59)
    - [0.6 - 2010-08-12 - **Minibuf-1**](#orgfbe24c2)
    - [0.5 - 2010-08-08 - Polishing](#orga6782df)


<a id="org4aa9944"></a>

# What is switch-window

switch-window is an emacs window switch tool, which offer a **visual** way to choose a window to switch to, delete, split or other operations.

![img](./snapshots/switch-window.png)


<a id="orgcc2e628"></a>

## Installation

1.  Config melpa source, please read: <http://melpa.org/#/getting-started>
2.  M-x package-install RET switch-window RET

Note: User can install switch-window with [El-Get](http://github.com/dimitri/el-get) too.


<a id="org1c89b8d"></a>

## Configure and Usage

    (require 'switch-window)
    (global-set-key (kbd "C-x o") 'switch-window)
    (global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
    (global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
    (global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
    (global-set-key (kbd "C-x 0") 'switch-window-then-delete)


<a id="org25d4be3"></a>

## Tips


<a id="org7393857"></a>

### I want to select a window with "a-z" instead of "1-9".

    (setq switch-window-shortcut-style 'qwerty)
    (setq switch-window-qwerty-shortcuts
          '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))


<a id="org4c3ee94"></a>

### I want to let window to show bigger label.

    (setq switch-window-increase 6) ;Increase or decrease this number.


<a id="orgf74e66a"></a>

### I want to **hide** window label when window's number < 3

    (setq switch-window-threshold 2)


<a id="orga354f88"></a>

### I want to select minibuffer with label "z".

    (setq switch-window-minibuffer-shortcut "z")


<a id="org80dee4a"></a>

### Switch-window seem to conflict with Exwm, how to do?

By default, switch-window get user's input with the help of function \`read-event', this approach does not work well with [Exwm](https://github.com/ch11ng/exwm) (Emacs X window manager), user should set the below variable and use minibuffer to get input instead:

    (setq switch-window-input-style 'minibuffer)


<a id="org31494d3"></a>

### I use text terminal, but I want **bigger** label.

The only choice is using asciiart, which **draw** a bigger label with **small** ascii char.

    (setq switch-window-shortcut-appearance 'asciiart)

![img](./snapshots/switch-window-3.png)


<a id="orge82fd4d"></a>

### I want to use image or icon as label.

1.  Prepare your label images, rename them to: 1.png &#x2026; 9.png, a.png &#x2026; z.png.

    You can use other image types supported by emacs, please see: \`image-types'.
2.  Put all above images to directory: \`switch-window-image-directory'.
3.  Set variable: \`switch-window-shortcut-appearance'

        (setq switch-window-shortcut-appearance 'image)

![img](./snapshots/switch-window-2.png)


<a id="org7d280af"></a>

### \`switch-window-shortcut-appearance' can't satisfy my need. how to do?

All you should do is hacking you own label buffer function, for example: my-switch-window-label-buffer-function, and set the below variable:

    (setq switch-window-label-buffer-function
          'my-switch-window-label-buffer-function)


<a id="org24c7efa"></a>

### Have any other similar package exist?

-   [ace-window](https://github.com/abo-abo/ace-window)


<a id="org7bea61e"></a>

## Changelog


<a id="orgf2a1ccd"></a>

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


<a id="org2049cc7"></a>

### 1.0.0 - 2015-01-14

-   Please fixme.


<a id="org27b1972"></a>

### 0.11 - 2013-09-14

-   restore point to end-of-buffer for windows where it was the case after switching, fixing an anoying bug.


<a id="org314849e"></a>

### 0.10 - 2011-06-19

-   implement M-x delete-other-window (thanks developernotes on github)


<a id="org03d1713"></a>

### 0.9 - 2010-11-11 - emacs22 called, it wants some support

-   implement a propertize based hack to support emacs22


<a id="orgd73ee54"></a>

### 0.8 - 2010-09-13 - 999

-   Suport more than 9 windows (with a single key to type)
-   Use quail-keyboard-layout to choose single key labels for windows


<a id="orga77bf59"></a>

### 0.7 - 2010-08-23 - window-dedicated-p

-   temporarily unset the window dedicated flag for displaying the numbers, patch from René Kyllingstad <Rene@Kyllingstad.com>
-   fix timeout and RET handling wrt to not changing window selection


<a id="orgfbe24c2"></a>

### 0.6 - 2010-08-12 - **Minibuf-1**

-   add support for selecting the minibuffer when it's active
-   some try at a better horizontal centering
-   assorted cleanup


<a id="orga6782df"></a>

### 0.5 - 2010-08-08 - Polishing

-   dim:switch-window-increase is now a maximum value


Converted from switch-window.el by [el2org](https://github.com/tumashu/el2org) .