- [What is switch-window](#orgf748bf4)
  - [Installation](#org2580067)
  - [Configure and Usage](#org671313f)
  - [Tips](#orgef960bc)
    - [I want to select a window with "a-z" instead of "1-9".](#org43edfba)
    - [I want to let window to show bigger label.](#org6a7a187)
    - [I want to **hide** window label when window's number < 3](#orgad2cd22)
    - [I want to select minibuffer with label "z".](#orgc932fff)
    - [I want to make switch-window beautiful?](#orgcf23dac)
  - [Changelog](#org728b245)
    - [0.11 - 2013-09-14](#orgdfb6415)
    - [0.10 - 2011-06-19](#org0eaf362)
    - [0.9 - 2010-11-11 - emacs22 called, it wants some support](#org59c7ea7)
    - [0.8 - 2010-09-13 - 999](#orgbc1dc7b)
    - [0.7 - 2010-08-23 - window-dedicated-p](#orgbcc8b8f)
    - [0.6 - 2010-08-12 - **Minibuf-1**](#org633e92e)
    - [0.5 - 2010-08-08 - Polishing](#org507437a)


<a id="orgf748bf4"></a>

# What is switch-window

switch-window is an emacs window switch tool, which offer a **visual** way to choose a window to switch to, delete, split or other operations.

![img](./snapshots/switch-window.png)


<a id="org2580067"></a>

## Installation

1.  Config melpa source, please read: <http://melpa.org/#/getting-started>
2.  M-x package-install RET switch-window RET

Note: User can install switch-window with [El-Get](http://github.com/dimitri/el-get) too.


<a id="org671313f"></a>

## Configure and Usage

    (require 'switch-window)
    (global-set-key (kbd "C-x o") 'switch-window)
    (global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
    (global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
    (global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
    (global-set-key (kbd "C-x 0") 'switch-window-then-delete)


<a id="orgef960bc"></a>

## Tips


<a id="org43edfba"></a>

### I want to select a window with "a-z" instead of "1-9".

    (setq switch-window-shortcut-style 'qwerty)
    (setq switch-window-qwerty-shortcuts
          '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))


<a id="org6a7a187"></a>

### I want to let window to show bigger label.

    (setq switch-window-increase 6) ;Increase or decrease this number.


<a id="orgad2cd22"></a>

### I want to **hide** window label when window's number < 3

    (setq switch-window-threshold 2)


<a id="orgc932fff"></a>

### I want to select minibuffer with label "z".

    (setq switch-window-minibuffer-shortcut "z")


<a id="orgcf23dac"></a>

### I want to make switch-window beautiful?

All you should to do is setting the variable \`switch-window-label-buffer-function', for example:

    (setq switch-window-label-buffer-function
          'my-switch-window-label-buffer-function)

The below are some switch-window user's showcases:

![img](./snapshots/switch-window-2.png) ![img](./snapshots/switch-window-3.png)


<a id="org728b245"></a>

## Changelog


<a id="orgdfb6415"></a>

### 0.11 - 2013-09-14

-   restore point to end-of-buffer for windows where it was the case after switching, fixing an anoying bug.


<a id="org0eaf362"></a>

### 0.10 - 2011-06-19

-   implement M-x delete-other-window (thanks developernotes on github)


<a id="org59c7ea7"></a>

### 0.9 - 2010-11-11 - emacs22 called, it wants some support

-   implement a propertize based hack to support emacs22


<a id="orgbc1dc7b"></a>

### 0.8 - 2010-09-13 - 999

-   Suport more than 9 windows (with a single key to type)
-   Use quail-keyboard-layout to choose single key labels for windows


<a id="orgbcc8b8f"></a>

### 0.7 - 2010-08-23 - window-dedicated-p

-   temporarily unset the window dedicated flag for displaying the numbers, patch from René Kyllingstad <Rene@Kyllingstad.com>
-   fix timeout and RET handling wrt to not changing window selection


<a id="org633e92e"></a>

### 0.6 - 2010-08-12 - **Minibuf-1**

-   add support for selecting the minibuffer when it's active
-   some try at a better horizontal centering
-   assorted cleanup


<a id="org507437a"></a>

### 0.5 - 2010-08-08 - Polishing

-   dim:switch-window-increase is now a maximum value


Converted from switch-window.el by [el2org](https://github.com/tumashu/el2org) .