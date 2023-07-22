#!/usr/bin/env python
'''
Web config
'''
AUTHOR = 'Austin'
SITENAME = 'Anardil'
SITEURL = 'https://www.anardil.net'

THEME = 'alchemy'
SITE_SUBTEXT = 'A spot of sunlight'
PAGES_ON_MENU = True
CATEGORIES_ON_MENU = True
TAGS_ON_MENU = True
SHOW_ARTICLE_AUTHOR = True
SHOW_HOME_ON_MENU = True

EMAIL_ADDRESS = 'austin@anardil.net'
GITHUB_ADDRESS = 'https://github.com/Gandalf-'


PATH = 'content'
STATIC_PATHS = ['blog', 'extra']

ARTICLE_PATHS = ['blog']
ARTICLE_SAVE_AS = '{date:%Y}/{slug}.html'
ARTICLE_URL = '{date:%Y}/{slug}.html'

EXTRA_PATH_METADATA = {
    'extra/favicon.ico': {'path': 'favicon.ico'},
    'extra/robots.txt': {'path': 'robots.txt'},
}

TIMEZONE = 'America/Los_Angeles'

DEFAULT_LANG = 'en'

# Feed generation is usually not desired when developing
FEED_ALL_ATOM = None
CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None
AUTHOR_FEED_ATOM = None
AUTHOR_FEED_RSS = None

FEED_DOMAIN = SITEURL
FEED_RSS = 'feeds/rss.xml'

# Blogroll
LINKS = (
    ('Pelican', 'http://getpelican.com/'),
    ('Python.org', 'http://python.org/'),
    ('Jinja2', 'http://jinja.pocoo.org/'),
)

# Social widget
SOCIAL = (
    ('Github', 'https://github.com/Gandalf-'),
    ('leaf@anardil.net', '#'),
)

DEFAULT_PAGINATION = 7

# Uncomment following line if you want document-relative URLs when developing
# RELATIVE_URLS = True
