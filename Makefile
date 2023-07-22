local:
	pelican --ignore-cache -o ~/working/object-publish/web

dev:
	pelican --autoreload -o ~/working/object-publish/web

publish:
	pelican --ignore-cache -o /mnt/ssd/hosts/web/www
