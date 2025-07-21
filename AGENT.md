# AGENT.md

## Project Overview
- **Game**: Elm recreation of the classic Java Miners4k game
- **Architecture**: Pure Elm frontend with Canvas rendering for pixel-perfect gameplay
- **Philosophy**: "If it compiles it works" - focus on compilation over testing
- **Rendering**: Canvas-based pixel manipulation to match original Java implementation exactly

## Build/Run Commands
- **Development**: `elm make src/Main.elm --output=main.js` then `npx serve`
- **Format**: `elm-format --yes src/`
- **Serve**: `npx serve` (serves on http://localhost:3000)
- **Requirements**: Elm must be installed (`npm install -g elm`)

## Current Implementation Status

### ✅ Core Complete Systems
- **Terrain Generation**: Java-accurate heightmap with subdivision algorithm
- **Miner AI & Physics**: Gravity, collision detection, slope walking, animation
- **Gold System**: Generation, collection, platform delivery with proper depth placement
- **Game State Management**: 7-level progression, timers, scoring, UI screens
- **Mouse Interaction**: Terrain digging/building with Java-accurate brushes
- **Camera System**: Arrow key scrolling with bounds checking

### ❌ Known Issues Needing Fix
- **Terrain Generation**: Colors/textures not accurate to original Java
- **Slime Growth**: Diamond-shaped expansion instead of natural circular growth
- **Performance**: Game slows down with many miners on screen

### ✅ Recently Fixed Major Bugs
- Random generation depth bias (gold/rocks now spawn at proper depths)
- Miner spawn distribution (half from each side)
- Gold platform drops (only on platforms above ground)
- Timer countdown system
- URL level selection for debugging

## Architecture
- **Frontend**: Pure Elm SPA with Canvas rendering
- **Pixel Storage**: Java-exact indexing system (`x | y << 10`)
- **Rendering**: Pixel-perfect Canvas manipulation
- **Game Loop**: 60fps animation with proper state management
- **Level Config**: Java-accurate formulas with special case overrides

## Code Style
- **Elm**: Standard conventions, pipeline decoding for JSON
- **Functional**: Immutable data structures, pure functions
- **Canvas**: Direct pixel manipulation for accuracy
- **Performance**: Efficient array operations and minimal allocations
