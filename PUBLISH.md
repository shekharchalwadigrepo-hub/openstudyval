# Publish this repository

Replace `YOU` and `you@example.com` before the first push.

```bash
cd openstudyval
git init -b main
git add .
git commit -m "Initial openstudyval 0.1.0"
gh repo create openstudyval --public --source=. --remote=origin --push
```

If `gh` is not authenticated:

```bash
gh auth login
```

or create an empty repository on GitHub and:

```bash
git remote add origin git@github.com:YOUR_USER/openstudyval.git
git push -u origin main
```

CRAN submission, after `R CMD check --as-cran` is clean:

```bash
R CMD build .
R CMD check --as-cran openstudyval_0.1.0.tar.gz
```

Upload the built tarball at https://cran.r-project.org/submit.html
