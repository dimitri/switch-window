- [What is switch-window](#orge85c33c)
  - [Installation](#org28d7427)
  - [Configure and Usage](#org6c1682b)
  - [Tips](#org50348ae)
    - [I want to select a window with "a-z" instead of "1-9".](#orga8afa63)
    - [I want to let window to show bigger label.](#orge049e38)
    - [I want to **hide** window label when window's number < 3](#orgd722f9d)
    - [I want to select minibuffer with label "z".](#org3668604)
    - [Switch-window seem to conflict with Exwm, how to do?](#org096cb30)
    - [I want to make switch-window beautiful?](#org1a7980f)
    - [Have any other similar package exist?](#org3d790fc)
  - [Changelog](#orgd25df81)
    - [1.5.0 - 2017-04-29](#orgaf0f16d)
    - [1.0.0 - 2015-01-14](#org2139249)
    - [0.11 - 2013-09-14](#orga539157)
    - [0.10 - 2011-06-19](#orgbdf1641)
    - [0.9 - 2010-11-11 - emacs22 called, it wants some support](#org88be693)
    - [0.8 - 2010-09-13 - 999](#org12035ed)
    - [0.7 - 2010-08-23 - window-dedicated-p](#org819bf56)
    - [0.6 - 2010-08-12 - **Minibuf-1**](#org8885054)
    - [0.5 - 2010-08-08 - Polishing](#org182f39d)


<a id="orge85c33c"></a>

# What is switch-window

switch-window is an emacs window switch tool, which offer a **visual** way to choose a window to switch to, delete, split or other operations.

![img](./snapshots/switch-window.png)


<a id="org28d7427"></a>

## Installation

1.  Config melpa source, please read: <http://melpa.org/#/getting-started>
2.  M-x package-install RET switch-window RET

Note: User can install switch-window with [El-Get](http://github.com/dimitri/el-get) too.


<a id="org6c1682b"></a>

## Configure and Usage

    (require 'switch-window)
    (global-set-key (kbd "C-x o") 'switch-window)
    (global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
    (global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
    (global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
    (global-set-key (kbd "C-x 0") 'switch-window-then-delete)


<a id="org50348ae"></a>

## Tips


<a id="orga8afa63"></a>

### I want to select a window with "a-z" instead of "1-9".

    (setq switch-window-shortcut-style 'qwerty)
    (setq switch-window-qwerty-shortcuts
          '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))


<a id="orge049e38"></a>

### I want to let window to show bigger label.

    (setq switch-window-increase 6) ;Increase or decrease this number.


<a id="orgd722f9d"></a>

### I want to **hide** window label when window's number < 3

    (setq switch-window-threshold 2)


<a id="org3668604"></a>

### I want to select minibuffer with label "z".

    (setq switch-window-minibuffer-shortcut "z")


<a id="org096cb30"></a>

### Switch-window seem to conflict with Exwm, how to do?

By default, switch-window get user's input with the help of function \`read-event', this approach does not work well with [Exwm](https://github.com/ch11ng/exwm) (Emacs X window manager), user should set the below variable and use minibuffer to get input instead:

    (setq switch-window-input-style 'minibuffer)


<a id="org1a7980f"></a>

### I want to make switch-window beautiful?

All you should to do is setting the variable \`switch-window-label-buffer-function', for example:

    (setq switch-window-label-buffer-function
          'my-switch-window-label-buffer-function)

The below are some switch-window user's showcases:

![img](./snapshots/switch-window-2.png) ![img](./snapshots/switch-window-3.png)


<a id="org3d790fc"></a>

### Have any other similar package exist?

-   [ace-window](https://github.com/abo-abo/ace-window)


<a id="orgd25df81"></a>

## Changelog


<a id="orgaf0f16d"></a>

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


<a id="org2139249"></a>

### 1.0.0 - 2015-01-14

-   Please fixme.


<a id="orga539157"></a>

### 0.11 - 2013-09-14

-   restore point to end-of-buffer for windows where it was the case after switching, fixing an anoying bug.


<a id="orgbdf1641"></a>

### 0.10 - 2011-06-19

-   implement M-x delete-other-window (thanks developernotes on github)


<a id="org88be693"></a>

### 0.9 - 2010-11-11 - emacs22 called, it wants some support

-   implement a propertize based hack to support emacs22


<a id="org12035ed"></a>

### 0.8 - 2010-09-13 - 999

-   Suport more than 9 windows (with a single key to type)
-   Use quail-keyboard-layout to choose single key labels for windows


<a id="org819bf56"></a>

### 0.7 - 2010-08-23 - window-dedicated-p

-   temporarily unset the window dedicated flag for displaying the numbers, patch from René Kyllingstad <Rene@Kyllingstad.com>
-   fix timeout and RET handling wrt to not changing window selection


<a id="org8885054"></a>

### 0.6 - 2010-08-12 - **Minibuf-1**

-   add support for selecting the minibuffer when it's active
-   some try at a better horizontal centering
-   assorted cleanup


<a id="org182f39d"></a>

### 0.5 - 2010-08-08 - Polishing

-   dim:switch-window-increase is now a maximum value


Converted from switch-window.el by [el2org](https://github.com/tumashu/el2org) .