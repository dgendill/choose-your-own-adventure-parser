# Choose Your Own Adventure Parser

Command line tool and codebase for parsing choose your own adventures files. [See choose-your-own-adventure-maker](https://github.com/dgendill/choose-your-own-adventure-maker) for the file specification and a user interface for making the files.

### Get Started

Checkout the repo and run `npm install`. To compile a choose-your-own-adventure-file run

```
node story-compiler.js adventure --file path/to/file.str --to dist
```

For example...

```
node story-compiler.js adventure --file examples/ShortStory.str --to dist
```

This will generate a static website in the dist folder, which you can view by running `npm run server -- dist`.

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
npm run compile-command-line
```

This will create `story-compiler.js` in the root folder.

---

To compile the front end driver, run `npm run compile`.  This will create the `main.js` in the dev folder.