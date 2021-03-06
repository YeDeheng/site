---
title: Hacking on coleslaw
date: 2013-04-19 00:15:32
tags: hack, common lisp, coleslaw, github, pages
---
_In this post, I document how I hacked [coleslaw](https://github.com/redline6561/coleslaw) to add [Github's pages](http://pages.github.com/) integration._

## What is coleslaw? ##
coleslaw is a static blog generator written in Common Lisp, much like [Jekyll](http://jekyllrb.com/), albeit with less features.

I've been using [Octopress](http://octopress.org/) to generate this site. Since I don't know [Ruby](http://ruby-lang.org/), and have to intention to learn it, I can't hack on Octopress's code nor on Jekyll's, which is a shame. As I am learning [Common Lisp](http://http://en.wikipedia.org/wiki/Common_Lisp/), and wanting to work on something in it, switching to coleslaw seems perfect for me.

## Trying coleslaw out ##
Here's how I set up and use coleslaw.

* Install [git](http://git-scm.com/)
* Set up a bare git repo

```bash
cd /path/to/repo # this is where the post-receive script is put
git --bare init
```

* Install Lisp (I choose [sbcl](http://sbcl.org/))
* Install [Quicklisp](http://quicklisp.org/)
* Get coleslaw's settings file

```bash
wget -c https://raw.github.com/redline6561/coleslaw/master/examples/single-site.coleslawrc \
     -O ~/.coleslawrc
```

* The settings must be edited accordingly. I find the naming a bit confusing, so I have put some comment below for clarification.

```commonlisp
(:author "Example Setter"
 :deploy "/path/to/deploy" ; this is where coleslaw writes its output
 :domain "http://blog.example.com"
 :feeds ("lisp")
 :plugins ((mathjax)
           (disqus :shortname "my-site-name")
           (analytics :tracking-code "foo"))
 ;; this is where coleslaw writes its temporary output, which gets moved
 ;; into :DEPLOY
 :repo "/path/to/some/temporary/folder/"
 :sitenav ((:url "http://example.com/" :name "Home")
           (:url "http://twitter.com/example" :name "Twitter")
           (:url "http://github.com/example" :name "Code"))
 :title "Blog title"
 :theme "hyde")
```

* Get `post-receive` hook script and edit the file according to the instructions inside

```bash
wget -c https://raw.github.com/redline6561/coleslaw/master/examples/example.post-receive \
     -O /path/to/repo/hooks/post-receive
# edit the file
```

* Make the script executable

```bash
chmod +x /path/to/repo/hooks/post-receive
```

* Create another repository to hold posts

```bash
cd /path/to/posts
git init
```

* Add the bare repo as a remote of the posts repo

```bash
git remote add prod file:///path/to/repo/
```

* Use a server of choice to serve the site. Here I choose [PHP](http://php.net/)'s built in server.

```bash
cd /path/to/deploy/.curr/
php -S localhost:8080
```

## Motivation ##
As I want to put my blog on Github's pages with my custom domain, I need the generator to put a file named `CNAME`, whose content is my custom domain name, in the root of the generated site. However, currently coleslaw only recognizes files with `post` extension, along with files in the `static` folder, and does not allow putting custom files in the root of the site. I need to hack up a way to generate that `CNAME` file under the root of the site.

## Forking the code ##
The usual workflow on Github is to fork the code, hack on it, commit and then send a pull request to the original author.

After forking the code and cloning it to my machine, I need to setup Quicklisp's quickproject according to [this](http://xach.livejournal.com/278047.html) so that it sees my code.

Since I have generated the posts using coleslaw, Quicklisp must have pulled the code from its repository. For safety, I uninstall Quicklisp's coleslaw.

```commonlisp
;; start sbcl and load Quicklisp's setup.lisp
(use-package :ql-dist)
(uninstall (release "coleslaw"))
; instructions gotten from
;   https://groups.google.com/forum/?fromgroups=#!topic/quicklisp/CvB3mwsL7l4
```

## Hack on it ##
I add a property to class `blog` in order to capture user's intent to integrate with Github's pages.

```commonlisp
; in src/config.lisp
(defclass blog ()
  (
   ; [...]
   (github :initarg :github :initform nil :accessor github)
   ; [...]
   ))
```

And then the code to actually generate the `CNAME` file

```commonlisp
(defgeneric deploy (staging)
  (((
        ; [...]
        (run-program "mv ~a ~a" staging new-build)
        (when (github *config*)
          (let ((cname-filename (rel-path "" "~a/CNAME" new-build))
                (stripped-url (subseq (domain *config*)
                                      (+ 2 (position #\/ (domain *config*))))))
            (with-open-file (cname cname-filename
                                   :direction :output
                                   :if-exists :supersede)
              (format cname "~a~%" stripped-url))))
        (when (probe-file prev)
        ; [...]
        )))))
```

## (Not) Send a pull request to the original author ##
Since the way I pulled out the domain name from the `blog` object is very ad-hoc, I don't think this code is production quality yet. I should write a separate library to parse the url and then use that to robustly extract the domain name. Until then, I won't send a pull request to the original author.
