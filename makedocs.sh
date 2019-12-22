
BUILDDIR=dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/mixfix-0.1.0.0/x/website/build/website/website.jsexe
SITEDIR=site

rm -f $SITEDIR/*
for f in index.html lib.js rts.js runmain.js out.js; do
    cp $BUILDDIR/$f $SITEDIR
done

git add $SITEDIR
git commit -m 'Updated github page'
git subtree push --prefix $SITEDIR origin gh-pages
