www = ~/working/object-publish/web

local:
	pelican -o ${www}

dev:
	pelican --autoreload -o ${www}

sync:
	rsync -av ${www}/ aspen:/mnt/ssd/hosts/web/www/

tags:
	@sh opt/show-tags.sh

serve:
	@serve ${www}
