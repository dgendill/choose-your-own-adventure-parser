# Choose Your Own Adventure Parser

Command line tool and codebase for parsing choose your own adventures files. [See choose-your-own-adventure-maker](https://github.com/dgendill/choose-your-own-adventure-maker) for the file specification and a user interface for making the files.

### Get Started

You'll need to have [pulp](https://github.com/purescript-contrib/pulp) and [PureScript](https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md) installed.

```
npm install -g purescript pulp
```

Then checkout the repo and run `npm install` in the project root. Then you can compile a choose-your-own-adventure-file by running...

```
node compiler.js --file path/to/file.str --to dist
```

For example...

```
node compiler.js --file examples/ShortStory.str --to dist
```

This will generate `dist/index.html` which can be viewed directly in the browser.

### JavaScript Library

The compiler can also be used on the front end. Include `dev/main.js` on the page.  To compile a story...

```
PS.Main.compile(text).then(function(parsedStory) {
  console.log(parsedStory);
}, function(err) {
  console.err(err);
});
```

### Development

To compile the command line tool run...

```
npm run buildcompiler
```

This will create `compiler.js` in the root folder.

---

To compile the front end driver, run `npm run compile`.  This will create the `main.js` in the dev folder.