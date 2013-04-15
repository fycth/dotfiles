ln -s .emacs ~/.emacs
ln -s .emacs.d ~/.emacs.d
ln -s .hgrc ~/.hgrc

git clone https://github.com/massemanet/distel.git .emacs.d/deps/distel
cd .emacs.d/deps/distel
make
cd ~/dotfiles

git clone https://github.com/rmm5t/maxframe.el.git .emacs.d/deps/maxframe
git clone https://github.com/sellout/emacs-color-theme-solarized.git .emacs.d/themes/solarized
wget ftp://ftp.gnu.org/gnu/tramp/tramp-2.2.5.tar.gz
tar -xzf tramp-2.2.5.tar.gz
mv tramp-2.2.2.tar.gz tramp

mkdir .emacs.d/backups
