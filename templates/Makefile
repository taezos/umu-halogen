build-all:
	spago build && npm install

build:
	spago build

build-watch:
	spago build --watch

bundle:
	spago bundle-app --main Main --to dist/app.js && ./node_modules/.bin/parcel build html/index.html

bundle-watch:
	spago bundle-app --main Main --to dist/app.js --watch && ./node_modules/.bin/parcel build html/index.html

clean:
	rm -rf .cache .spago node_modules .psci_modules output dist

start:
	./node_modules/.bin/http-server dist

bundle-start:
	make bundle && make start
