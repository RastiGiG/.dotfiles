;;; configuration for elfeed

(require 'elfeed)

(setq elfeed-feeds
      '(("https://www.archlinux.org/feeds/news/" linux distro)
        ("https://planet.emacslife.com/atom.xml" emacs community)
        ("https://www.ecb.europa.eu/rss/press.html" economics eu)
        ("http://planetpython.org/rss20.xml" python)
        ("http://planet.scipy.org/rss20.xml" python)
        ("http://planet.emacsen.org/atom.xml" emacs)
        ;; Blogs
        ("https://irreal.org/blog/?feed=rss2" emacs blogs)
        ("https://www.kuketz-blog.de/feed/" it blogs security privacy)
        ("https://www.dkriesel.com/feed.php?linkto=current&content=html&mode=blogtng&blog=blog-de" it blogs security data-science)
        ;; Podcasts
        ("http://feed.pippa.io/public/shows/teamhuman" podcasts culture)
        ("https://feeds.buzzsprout.com/1875696.rss" podcasts culture politics)
        ("https://wakingup.libsyn.com/rss" podcasts culture politics)
        ("https://feeds.lagedernation.org/feeds/ldn-aac.xml" podcasts culture politics)
        ("https://neuezwanziger.de/feed/mp3/" podcasts culture politics)
        ("https://alias-podcast.de/feed/mp3/" podcasts culture politics)
        ("https://feeds.transistor.fm/the-drug-science-podcast" podcasts science medicine)
        ("https://logbuch-netzpolitik.de/feed/m4a" podcasts it culture politics)
        ("https://www.netzpolitik.org/category/netzpolitik-podcast/feed/itunes" podcasts it culture politics)
        ;; Entertainment
        ;;;;; Webcomics
        ("http://nedroid.com/feed/" webcomic)
        ("https://xkcd.com/atom.xml" webcomic)
        ))

(defface python-elfeed-entry
  '((t :background "ff00ff"))         ;; prev: "Darkseagreen1"
  "Marks a python Elfeed entry."
  :group 'personal-elfeed)

(defface emacs-elfeed-entry
  '((t :background "008888"))               ;; prev: "Lightblue1"
  "Marks a Emacs Elfeed entry."
  :group 'personal-elfeed)

(defface blogs-elfeed-entry
  '((t :background "7fe757"))
  "Marks a Blog Elfeed entry."
  :group 'personal-elfeed)

(defface podcasts-elfeed-entry
  '((t :background "941f27"))
  "Marks a Podcast Elfeed entry."
  :group 'personal-elfeed)

(push '(python python-elfeed-entry)
      elfeed-search-face-alist)

(push '(emacs emacs-elfeed-entry)
      elfeed-search-face-alist)

(push '(blogs blogs-elfeed-entry)
      elfeed-search-face-alist)

(push '(podcasts podcasts-elfeed-entry)
      elfeed-search-face-alist)
