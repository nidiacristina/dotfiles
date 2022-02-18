`git.el` and `git-blame.el` from Git v2.17.5.  These provide the legacy
`git-status` and `git-blame` functions, respectively, which were removed in 2018
to promote migration to other tools such as Magit.  For simple workflows, these
work just fine and are preserved here until the users of these configuration
files change their habits.

**NOTE:** These will shadow the system's files for older distributions.  I don't
believe that is an issue, though be aware in case of observing anything strange.

These come unmodified from the official Git distribution and are provided under
the GPL v2 license.  See the `LICENSE` file for details.
