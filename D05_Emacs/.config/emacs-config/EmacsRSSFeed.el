;;; configuration for elfeed

(require 'elfeed)

(setq elfeed-feeds
      '(("https://www.archlinux.org/feeds/news/" it linux distro)
        ("https://planet.emacslife.com/atom.xml" it emacs community)
        ("https://www.ecb.europa.eu/rss/press.html" it economics eu)
        ("http://planetpython.org/rss20.xml" it python)
        ("http://planet.scipy.org/rss20.xml" it python)
        ("http://planet.emacsen.org/atom.xml" it emacs)
        ("https://ctan.org/ctan-ann/atom" it tex)
        ;; Blogs
        ("https://irreal.org/blog/?feed=rss2" it emacs blogs)
        ("https://www.kuketz-blog.de/feed/" it blogs security privacy)
        ("https://komascript.de/node/feed" it blogs tex) ("https://www.dkriesel.com/feed.php?linkto=current&content=html&mode=blogtng&blog=blog-de" it blogs security data-science)
        ("https://latex-ninja.com/feed/" it blogs tex)
        ("https://zettelkasten.de/feed.atom" blogs methodology zettelkasten)
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
        ;;;;; Videos/YouTube
        ;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g" personal video)
        ))

(defface python-elfeed-entry
  '((t :background "#ForestGreen"))         ;; prev: "Darkseagreen1"
  "Marks a python Elfeed entry."
  :group 'personal-elfeed)

(defface tex-elfeed-entry
  '((t :background "#LimeGreen"))         
  "Marks a python Elfeed entry."
  :group 'personal-elfeed)

(defface emacs-elfeed-entry
  '((t :background "SpringGreen"))               ;; prev: "Lightblue1"
  "Marks a Emacs Elfeed entry."
  :group 'personal-elfeed)

(defface blogs-elfeed-entry
  '((t :background "Maroon"))
  "Marks a Blog Elfeed entry."
  :group 'personal-elfeed)

(defface podcasts-elfeed-entry
  '((t :background "MediumVioletRed"))
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

(push '(tex tex-elfeed-entry)
      elfeed-search-face-alist)
