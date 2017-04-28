- [What is switch-window](#orgb9155f0)
  - [Installation](#orge7e0868)
  - [Configure and Usage](#orgb4079cb)
  - [Tips](#orgf244670)
    - [I want to select a window with "a-z" instead of "1-9".](#orgc11ac21)
    - [I want to let window to show bigger label.](#org4f36c6d)
    - [I want to **hide** window label when window's number < 3](#orgb4427a7)
    - [I want to select minibuffer with label "z".](#org114bfee)
    - [Switch-window seem to conflict with Exwm, how to do?](#org28aba4e)
    - [I want to make switch-window beautiful?](#org9517850)
    - [Have any other similar package exist?](#org7ec9ab3)
  - [Changelog](#orgc554e94)
    - [0.11 - 2013-09-14](#org737bf59)
    - [0.10 - 2011-06-19](#orgc074b1f)
    - [0.9 - 2010-11-11 - emacs22 called, it wants some support](#orgef1c9f8)
    - [0.8 - 2010-09-13 - 999](#orge4d8ca3)
    - [0.7 - 2010-08-23 - window-dedicated-p](#org17a8108)
    - [0.6 - 2010-08-12 - **Minibuf-1**](#orgcd0840d)
    - [0.5 - 2010-08-08 - Polishing](#org707e187)


<a id="orgb9155f0"></a>

# What is switch-window

switch-window is an emacs window switch tool, which offer a **visual** way to choose a window to switch to, delete, split or other operations.

![img](./snapshots/switch-window.png)


<a id="orge7e0868"></a>

## Installation

1.  Config melpa source, please read: <http://melpa.org/#/getting-started>
2.  M-x package-install RET switch-window RET

Note: User can install switch-window with [El-Get](http://github.com/dimitri/el-get) too.


<a id="orgb4079cb"></a>

## Configure and Usage

    (require 'switch-window)
    (global-set-key (kbd "C-x o") 'switch-window)
    (global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
    (global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
    (global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
    (global-set-key (kbd "C-x 0") 'switch-window-then-delete)


<a id="orgf244670"></a>

## Tips


<a id="orgc11ac21"></a>

### I want to select a window with "a-z" instead of "1-9".

    (setq switch-window-shortcut-style 'qwerty)
    (setq switch-window-qwerty-shortcuts
          '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))


<a id="org4f36c6d"></a>

### I want to let window to show bigger label.

    (setq switch-window-increase 6) ;Increase or decrease this number.


<a id="orgb4427a7"></a>

### I want to **hide** window label when window's number < 3

    (setq switch-window-threshold 2)


<a id="org114bfee"></a>

### I want to select minibuffer with label "z".

    (setq switch-window-minibuffer-shortcut "z")


<a id="org28aba4e"></a>

### Switch-window seem to conflict with Exwm, how to do?

By default, switch-window get user's input with the help of function \`read-event', this approach does not work well with [Exwm](https://github.com/ch11ng/exwm) (Emacs X window manager), user should set the below variable and use minibuffer to get input instead:

    (setq switch-window-input-style 'minibuffer)


<a id="org9517850"></a>

### I want to make switch-window beautiful?

All you should to do is setting the variable \`switch-window-label-buffer-function', for example:

    (setq switch-window-label-buffer-function
          'my-switch-window-label-buffer-function)

The below are some switch-window user's showcases:

![img](./snapshots/switch-window-2.png) ![img](./snapshots/switch-window-3.png)


<a id="org7ec9ab3"></a>

### Have any other similar package exist?

-   [ace-window](https://github.com/abo-abo/ace-window)


<a id="orgc554e94"></a>

## Changelog


<a id="org737bf59"></a>

### 0.11 - 2013-09-14

-   restore point to end-of-buffer for windows where it was the case after switching, fixing an anoying bug.


<a id="orgc074b1f"></a>

### 0.10 - 2011-06-19

-   implement M-x delete-other-window (thanks developernotes on github)


<a id="orgef1c9f8"></a>

### 0.9 - 2010-11-11 - emacs22 called, it wants some support

-   implement a propertize based hack to support emacs22


<a id="orge4d8ca3"></a>

### 0.8 - 2010-09-13 - 999

-   Suport more than 9 windows (with a single key to type)
-   Use quail-keyboard-layout to choose single key labels for windows


<a id="org17a8108"></a>

### 0.7 - 2010-08-23 - window-dedicated-p

-   temporarily unset the window dedicated flag for displaying the numbers, patch from René Kyllingstad <Rene@Kyllingstad.com>
-   fix timeout and RET handling wrt to not changing window selection


<a id="orgcd0840d"></a>

### 0.6 - 2010-08-12 - **Minibuf-1**

-   add support for selecting the minibuffer when it's active
-   some try at a better horizontal centering
-   assorted cleanup


<a id="org707e187"></a>

### 0.5 - 2010-08-08 - Polishing

-   dim:switch-window-increase is now a maximum value


Converted from switch-window.el by [el2org](https://github.com/tumashu/el2org) .