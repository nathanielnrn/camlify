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
2.You now make local changes on your local branch "branch-name."
3.When you are fairly certain the code is decent (doesn't have to be perfect)
  Use [git push origin <branch-name>] (NOT MAIN). This tells git to take
  your local branch and uploads it to a new branch in the remote repository.
4.You can keep pushing to this remote version of your branch using
  [git push origin <branch-name>]

  Now we can go to the github repo and your web browser and go to "branches."
  Now make a pull request, this should let all team members see the changes.
  Anyone can confirm pull requests.

  We can also skip this test and directly push to main:
5.When your code is ready to be merged to the "good" branch (meaning everything)
  works) swtich to your local main using [git switch main].
6.Pull from remote main using [git pull origin main]
7.Merge your feature branch to main using [git merge <branch-name>
8.Fix any merges conflicts
9.Push your new updated version of main to the remote repo using
  [git push origin main]
10.Done!

To recap, when making changes on a certain feature:
1.[git switch <branch-name>]
2.[git pull origin main]
3.Make changes, add and commit as needed to <branch-name>.
4.[git push origin <branch-name>]
5.At this point make a pull request on github
  OR
  When happy use [git switch main] -> [git pull origin main]
6.[git merge <branch-name>]
7.[git push origin main]

Please make sure to only commit 100% working code to remote main (step 6)

  

That should be it.


TODO: Make sure you are working on your own branch (as detailed above)
Add your name here. When done, commit, pull, and push to git.
(We can check if the git repo works this way.)
Nathaniel Navarro

bin: location of our "main" ml file, which is turned into our main executable.
     Modules in lib directory can be accessed through Camlify.Mod where Mod is the name of the module

test: contains tests for project, must eventually set up OUnit

src: main directory containing most of our actual modules. Should contain .ml and .mli files

data: eventual storage of "config" files which detail info about playlists, albums, etc.
