Github Tutorial
================

## 1. Clone the Repository

Run this **once** to get a local copy of the project.

``` bash
git clone https://github.com/<username>/<repo-name>.git
cd <repo-name>
```

## 2. Create a New Branch for Your Work

Always branch off from `main` to keep the main codebase clean.

``` bash
git checkout main                 # Start from the latest main
git pull origin main             # Make sure it's up to date
git checkout -b feature/my-task  # Create and switch to your feature branch
```

Use a descriptive branch name like:

``` bash
git checkout -b feature/add-login-form
```

## 3. Add and Commit Changes

After making changes to files:

``` bash
git add .   # or git add <file-name> for specific files
```

Then commit with a message:

``` bash
git commit -m "Add login form" -m "Created a responsive login form with HTML and CSS. Includes validation and placeholder links for signup/forgot password."
```

- The **first `-m`** is a short summary.
- The **second `-m`** is a detailed explanation (optional but highly
  encouraged).

## 4. Push Your Branch to GitHub

Push your branch so it’s visible to others:

``` bash
git push origin feature/add-login-form
```

## 5. Open a Pull Request (PR)

1.  Visit your repo on GitHub.

2.  Click **“Compare & pull request”**.

3.  Add:

    - A **title** (short summary of the change).
    - A **description** (what you did, why, any dependencies).

4.  On the right, under **Reviewers**, **add your teammate**.

5.  Click **“Create pull request”**.

## 6. Review and Merge

Once the reviewer has approved the pull request:

- Click **“Merge pull request”**.
- Then click **“Delete branch”** to keep things tidy.

## 7. Keep Your Local `main` Updated

Before working on a new task:

``` bash
git checkout main
git pull origin main
```

This ensures your next feature starts from the latest version.

## Optional Clean-Up: Delete Local Branch

After merging a feature, remove the local branch:

``` bash
git branch -d feature/add-login-form
```

## Other Useful Git Actions

Action \| Command Example \| Description \|  
 \| - \| - \|  
Check current branch \| `git branch` \| Shows all local branches \|  
See repo status \| `git status` \| Shows changes and staged files \|  
See commit history \| `git log --oneline` \| One-line view of commit
history \|  
Undo last commit (local) \| `git reset --soft HEAD~1` \| Keeps changes,
removes last commit \|  
Stash unfinished work \| `git stash` \| Temporarily save changes without
committing \|  
Apply stashed changes \| `git stash pop` \| Reapply last stashed work \|

## Best Practices

- **One task per branch**: Easier to review and track.
- **Commit often**: Small, meaningful commits help track progress.
- **Pull before you push**: Avoid conflicts.
- **Use descriptive messages**: Tell your teammate what changed and why.
