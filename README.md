<div align="center">

<h1>Antics</h1>

<p>An Emacs package for running processes in the background.</p>
</div>
<hr />

## Installation

Clone this repository:

```sh
git clone https://github.com/johncoder/antics
```

Then, add this to your emacs configuration:

```lisp
(load-file "~/path/to/antics/antics.el")
```

## Configuration Files

To configure antics, create a text file and define the processes you'd like to run. Example:

```lisp
'(:name "My Processes"
  :items
  ("List some files"
   :cwd "~/"
   :cmd "ls -la"))
```

When you run `M-x antics RET` it will prompt you to choose the file you just created. It then opens a tabulated list of these processes and allow you to manage them.

| key binding | description |
| ----------- | ----------- |
| `g` | Refresh the list of processes |
| `R` | Load a configuration file `C-u` to force |
| `v` | View a process buffer |
| `RET` | Start an item and view its buffer |
| `s` | Start a process |
| `k` | Kill a process |
| `d` | Delete a process |
| `P` | View the process list (`list-processes`) |

Key map in the buffer for a process:

| key binding | description |
| ----------- | ----------- |
| `q` | Quit the window |
| `g` | Rerun the process |
| `k` | Kill the process |
