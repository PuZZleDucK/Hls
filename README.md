
# Haskell clone of coreutils #

This has turned into a vain attempt to recreate the GNU coreutils programs in Haskell, for no particular reason... Maybe someday some student called Hlinus will write a kernel and this will then come in handy.

## Progress so far: ##

### Done(ish): ###
* yes ==> Hyes (done)
* whoami ==> Hwhoami (done)
* basename ==> Hbasename (in principle done, some formatting todo)
* ls ==> Hls (work in progress, needs to work on real IO, colors, formats...)

### ToDo - Interesting: ###
* users
* true / false
* tr
* shred
* printf (prereq for many others)
* mkfifo / mknod / mktemp
* md5sum / cksum / sum
* link / ln / unlink
* kill
* env
* dd
* df
* chroot
* sleep
* date
### ToDo - Easy: ###
* uniq / seq / shuf / sort
* printenv / pwd / uptime / groups / hostid / hostname
* uname / uname-arch / uname-uname
* truncate / tail / head / echo / cat
* touch
* split
* rm / rmdir / mv / mkdir / cp / chgrp / chmod / chown
* nice
* du
* wc (prototype done)

### Hard - Have other prerequisites: ###
* tee
* stty
* stat
* stdbuf

### Unknown - What do they even do?: ###
* who
* unexpand
* tsort
* tty
* test
* timeout
* sync
* tac
* tac-pipe
* runcon
* selinux
* readlink
* realpath
* relpath
* remove
* prog-fprintf
* ptx
* nl
* nohup
* nproc
* numfmt
* od
* operand2sig
* paste
* pathchk
* pinky
* pr
* ls-dir / ls-ls / ls-vdir
* make-prime-list
* logname
* find-mount-point
* fmt
* fold
* getlimits
* group-list
* id
* install
* ioblksize
* join
* lbracket
* libstdbuf
* expand
* expr
* extent-scan
* factor
* dircolors
* dirname
* cp-hash
* csplit
* cut
* comm
* copy
* base64
* chcon


Major deps:
* System.Console.Terminfo.Cursor
* System.Environment


