# vue3-template
This is a building block for a vue3 project. Using this template you should be able to get a vue project up and running in minutes.

# Structure
- /public - contains [static elements](https://cli.vuejs.org/guide/html-and-static-assets.html) such as the favicon and index.html files.
- /src - Contains the components that vue  will compile into a SAP
	- /assets - Container for images, videos, etc.
  - /components - Container for .vue files. These are not to be routed directly to and should only be included in a parent component.
    - /common - Commonly used components that are pulled into other vue components. 
    - /modals - Components for pop-up overlay modals.
  - /global - Global files such as javascript or styles that can be included from many places
    - /plugins - Javscript files for commonly used scripts
    - /styles - SASS (or CSS, SCSS if you really want) files for common stylings
  - /router - Vue router container for all your SAP pathing needs. `index.js` here determines all of the differnt pages and how they're routed to
  - /views - Main .vue files. These are the parents to which you should route pages and include components within. Highly recommend building a folder structure within to reflect the site map for ease of navigation

# Key Caveats / Constraints
- Are there any 'gotchas' or constraints that the code requires? 
- Specific server versions, or versions of the server / code? For example, do you need a specific Azure server version, a specific Nodejs version, etc.?
## Plugins
### Copy To Clipboard
Global function to copy a body of text into a user's clipboard. This used to be more complicated before `navigator.clipboard` was a thing, but it's at least still slimming down code.
```js
this.$copyToClipboard("String to copy")
```

### Modals
Global function to show or hide a modal. Specific modals are created as vue components and should be stored within `/src/components/modals/` and then included in that directory's `Main.vue`.
```js
// show modal
this.$modals.show({name: "modalName", ...otherOptions})
// hide modal, hides all if no name is given
this.$modals.hide("modalName")
```

### Regex
A singular place to validate strings against some regex. A few common uses are provided, just add more to `const regexes` to make use of it
```js
this.$regex("String to check", regexType)
```

### Time
Assists in displaying date/time in different formats. Takes in a `Date` variable or timestamp
```js
//time stamp example
this.$buildDate(1651599475296, "{longMonth} {date}{dateEnding}, {year}, {hour}:{minutes} {meridian}")
// result: May 3rd, 2022, 1:37 pm
```

### Toast
Simple toast pop-up messages. Slides in/out from the bottom of the screen. Style up `components/common/Toast.vue` as you see fit.
```js
this.$toast("String to display")
// pass more options in such as time(ms)
this.$toast({copy: "String to display", time: 3000})
```

## Customize vue configuration
See [Configuration Reference](https://cli.vuejs.org/config/).

# How to deploy
1. Deployment step 1
2. Deployment step 2
3. Etc
4. 

# Get up and running
## Project setup
```
npm install
```
### Compiles and hot-reloads for development at localhost:3000
```
npm run serve
```
### Compiles and minifies for production
```
npm run build
```
### Lints and fixes files for a standardized look and feel
```
npm run lint
```