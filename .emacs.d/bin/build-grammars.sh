#!/bin/bash
set -e

echo "🌱 Setting up Tree-sitter grammars for Emacs..."

# Base directories
BASE_DIR="$HOME/.emacs.d/tree-sitter"
LIB_DIR="$BASE_DIR/lib"
INC_DIR="$BASE_DIR/include/tree_sitter"

mkdir -p "$LIB_DIR"
mkdir -p "$INC_DIR"

echo "📥 Downloading parser.h..."
curl -sSL https://raw.githubusercontent.com/tree-sitter/tree-sitter/master/lib/src/parser.h -o "$INC_DIR/parser.h"

cd "$BASE_DIR/manual"

build_grammar () {
  local name=$1
  local repo=$2
  local subdir=$3
  local lang_dir="$BASE_DIR/manual/$repo/$subdir"

  echo "📦 Building grammar for $name..."

  git clone --depth=1 https://github.com/tree-sitter/$repo.git "$BASE_DIR/manual/$repo" || true
  cd "$BASE_DIR/manual/$repo"
  git submodule update --init --recursive || true

  cd "$lang_dir"
  tree-sitter generate

  if [[ -f src/scanner.c ]]; then
    gcc -I "$BASE_DIR/include" -shared -o "libtree-sitter-$name.dylib" src/parser.c src/scanner.c -fPIC
  elif [[ -f src/scanner.cc ]]; then
    g++ -I "$BASE_DIR/include" -shared -o "libtree-sitter-$name.dylib" src/parser.c src/scanner.cc -fPIC
  else
    gcc -I "$BASE_DIR/include" -shared -o "libtree-sitter-$name.dylib" src/parser.c -fPIC
  fi

  cp "libtree-sitter-$name.dylib" "$LIB_DIR/"
  echo "✅ $name done."
}

build_grammar "typescript" "tree-sitter-typescript" "typescript"
build_grammar "tsx"        "tree-sitter-typescript" "tsx"
build_grammar "javascript" "tree-sitter-javascript" "."
build_grammar "c" "tree-sitter-c" "."

echo "🎉 All grammars built and installed to $LIB_DIR"
