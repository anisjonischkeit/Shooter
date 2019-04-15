# Reprocessing Shooter [![Netlify Status](https://api.netlify.com/api/v1/badges/d91b0b5d-ead3-4081-a759-7880199f04ef/deploy-status)](https://app.netlify.com/sites/determined-haibt-e6bd07/deploys)

## How to

```
git clone https://github.com/anisjonischkeit/Shooter.git
```

### Install

```
npm install
```

### Build

```
npm run build
```

### Start

```
npm start
```

To build to JS run `npm run build:web` and then run a static server, like `python -m SimpleHTTPServer` and go to `localhost:8000`. If you're using safari you can simply open the `index.html` and tick `Develop > Disable Cross-Origin Restrictions`.

To build to native run `npm run build:native` and run `npm run start:native`

The build system used is [bsb-native](https://github.com/bsansouci/bucklescript).
