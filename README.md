<h1 align="center">Project Treemacs</h1>
<hr>
<p align="center">
Simple treemacs backend for project.el:

* Projects in Treemacs map to project.el projects.
* Workspaces in Treemacs map to project.el projects + their external roots.
* `project-find-file`, `project-switch-to-buffer`, `project-find-regexp` et al operate on the current treemacs project.
* `project-or-external-find-file`, `project-or-external-find-regexp` operate on the current treemacs workspace.
</p>
<hr>

# Installing
Install `project-treemacs` directly from [MELPA](https://melpa.org/#/project-treemacs)

# Configuration
Manually:

``` emacs-lisp
(with-eval-after-load 'treemacs
  (require 'project-treemacs)
  (project-treemacs-mode))
```

With `use-package`:
``` emacs-lisp
(use-package project-treemacs
  :demand t
  :after treemacs
  :config
  (project-treemacs-mode))
```

# Usage
Enable the mode by calling `project-treemacs-mode`.

Use treemacs as you normally would, creating workspaces, projects, etc. 

Use project.el commands as you normally would, with your current project.el project defined by the current treemacs project, and your project external roots defined by the current treemacs workspace.

# Commands
* `project-treemacs-mode` - enables and disables the `project-treemacs` backend.

# User Options
* `project-treemacs-ignores` - list of patterns to add to `project-ignores`.
* `project-treemacs-prefer-backend` - Whether or not to prefer the project-treemacs backend over others. See the commentary in project.el for more information, and the documentation for `project-find-functions` in particular.

# Limitations
For the moment, the project-treemacs backend depends on the treemacs side panel being visible. 

For the moment, `project.el` commands are still based on the buffer from which they are called, although the latest version of `project.el` includes a TODO for allowing the user to "pick the current project for the whole Emacs session, independent of the current directory".

# Examples
![treemacs-project-search](https://github.com/cmccloud/project-treemacs/blob/master/examples/project-search.gif?raw=true)
Using `project-find-file` to search for files within the current treemacs project.
<hr>

![treemacs-workspace-search](https://github.com/cmccloud/project-treemacs/blob/master/examples/workspace-search.gif?raw=true)
Using `project-or-external-find-file` to search for files within the current treemacs workspace.


# Links
* [Working with Projects - Emacs Manual](https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html)

* [project.el at Emacs Mirror](https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/project.el)



