Installation steps:

Make a directory of your choosing from the terminal.
From there run [git clone git@github.coecis.cornell.edu:nrn25/camlify.git]
This should populate the directory with all files from the git repo.
If it does not, something went wrong.

At this point, try creating a new branch by running [git switch -c <new-branch-name>]
git switch switches to a branch that you also (-c)reated. Branch name should be
descriptive about content of work done on branch, versus just your name.

This is (kind of) like your own, localized version of any files and changes
from now on, before continuing any work you should make sure:
1.You're the branch you want to work on (by using [git switch <branch-name>])
2.Your branch is up to date with main by using [git pull origin] this syncs
  changes on the "working" version of the software with your local files.
  any merge conflicts should be shown and dealt with.

After working on any code:

1.Add changed files using [git add <file>]
2.Commit *good code* using [git commit]
3.Use [git pull origin] and make sure nothing newly introduced breaks
what you wrote.
4.Once you have done EVERYTHING above and you are *CERTAIN* your code works
and doesn't break anything you can [git push origin main]. This takes your
local version of code and uploads it to the main remote repository hosted
on Cornell's GitHub.

That should be it.


TODO: Add your name here. When done, commit and push to git. (We can check if the git repo works this way.)


bin: location of our "main" ml file, which is turned into our main executable.
     Modules in lib directory can be accessed through Camlify.Mod where Mod is the name of the module

test: contains tests for project, must eventually set up OUnit

src: main directory containing most of our actual modules. Should contain .ml and .mli files

data: eventual storage of "config" files which detail info about playlists, albums, etc.
