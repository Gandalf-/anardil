local:
	pelican -o ~/working/object-publish/web

dev:
	pelican --autoreload -o ~/working/object-publish/web

sync:
	rsync -av ~/working/object-publish/web/ aspen:/mnt/ssd/hosts/web/www/

tags:
	@sh opt/show-tags.sh
