vim9script

if exists('b:did_mcge_rust_ftplugin')
  finish
endif
b:did_mcge_rust_ftplugin = true

setlocal makeprg=cargo\ build
