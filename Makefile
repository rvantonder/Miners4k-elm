all:
	@npx serve -s . -l 3000

prod:
	elm make src/Main.elm --optimize --output=public/dist/main.js

dev:
	elm-live --port 3000 --start-page=index.html src/Main.elm -- --output=public/dist/main.js

clean:
	@rm -rf elm-stuff


.PHONY: all dev prod clean
