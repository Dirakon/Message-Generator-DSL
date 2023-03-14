spago build

# Use the preferred export
esbuild ./src/Main.js --bundle --target=esnext > bundledMessageGenerator.js
# esbuild ./src/Main.js --bundle --platform=node > bundledMessageGenerator.js
# or some other