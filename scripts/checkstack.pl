# Tool heavily inspired (almost fully copied) from Linux, where it is
# copyrighted as such:
#
# SPDX-License-Identifier: GPL-2.0
#   Copyright Joern Engel <joern@lazybastard.org>
#   Inspired by Linus Torvalds
#   Original idea maybe from Keith Owens
#   s390 port and big speedup by Arnd Bergmann <arnd@bergmann-dalldorf.de>
#   Mips port by Juan Quintela <quintela@mandrakesoft.com>
#   IA64 port via Andreas Dilger
#   Arm port by Holger Schurig
#   Random bits by Matt Mackall <mpm@selenic.com>
#   M68k port by Geert Uytterhoeven and Andreas Schwab
#   AArch64, PARISC ports by Kyle McMartin
#   sparc port by Martin Habets <errandir_news@mph.eclipse.co.uk>
#   ppc64le port by Breno Leitao <leitao@debian.org>
#   riscv port by Wadim Mueller <wafgo01@gmail.com>
#
# Thank you all so much!
#
# Usage:
# objdump -d {image} | perl scripts/checkstack.pl target

use strict;

# check for arch
#
# $re is used for two matches:
# $& (whole re) matches the complete objdump line with the stack growth
# $1 (first bracket) matches the size of the stack growth
#
# $dre is similar, but for dynamic stack redutions:
# $& (whole re) matches the complete objdump line with the stack growth
# $1 (first bracket) matches the dynamic amount of the stack growth
#
# $sub: subroutine for special handling to check stack usage.
#
# use anything else and feel the pain ;)
my (@stack, $re, $dre, $sub, $x, $xs, $funcre);
{
    my $arch = shift;
    if ($arch eq "") {
        print("architecture was not passed \n");
        exit
    }

    $x = "[0-9a-f]"; # hex character
    $xs = "[0-9a-f ]"; # hex character or space
    $funcre = qr/^$x* <(.*)>:$/;
    if ($arch eq 'aarch64-stivale2') {
        # ffffffc0006325cc:       a9bb7bfd        stp     x29, x30, [sp, #-80]!
        #             a110:       d11643ff        sub     sp, sp, #0x590
        $re = qr/^.*stp.*sp, \#-([0-9]{1,8})\]\!/o;
        $dre = qr/^.*sub.*sp, sp, #(0x$x{1,8})/o;
    } elsif ($arch eq 'arm-raspi2b') {
        # c0008ffc:	e24dd064	sub	sp, sp, #100	; 0x64
        $re = qr/.*sub.*sp, sp, #([0-9]{1,4})/o;
        $sub = \&arm_push_handling;
    } elsif ($arch eq 'sparc-leon3') {
        # f0019d10:       9d e3 bf 90     save  %sp, -112, %sp
        $re = qr/.*save.*%sp, -(([0-9]{2}|[3-9])[0-9]{2}), %sp/o;
    } elsif ($arch eq 'x86_64-limine') {
        # c0105234:       81 ec ac 05 00 00       sub    $0x5ac,%esp
        #     2f60:    48 81 ec e8 05 00 00       sub    $0x5e8,%rsp
        $re = qr/^.*[as][du][db]    \$(0x$x{1,8}),\%(e|r)sp$/o;
        $dre = qr/^.*[as][du][db]    (%.*),\%(e|r)sp$/o;
    } else {
        print("wrong or unknown target \"$arch\"\n");
        exit
    }
}

#
# To count stack usage of push {*, fp, ip, lr, pc} instruction in ARM,
# if FRAME POINTER is enabled.
# e.g. c01f0d48: e92ddff0 push {r4, r5, r6, r7, r8, r9, sl, fp, ip, lr, pc}
#
sub arm_push_handling {
    my $regex = qr/.*push.*fp, ip, lr, pc}/o;
    my $size = 0;
    my $line_arg = shift;

    if ($line_arg =~ m/$regex/) {
        $size = $line_arg =~ tr/,//;
        $size = ($size + 1) * 4;
    }

    return $size;
}

#
# main()
#
my ($func, $file, $lastslash, $total_size, $addr, $intro);

$total_size = 0;

while (my $line = <STDIN>) {
    if ($line =~ m/$funcre/) {
        $func = $1;
        next if $line !~ m/^($xs*)/;
        if ($total_size > 0) {
            push @stack, "$intro$total_size\n";
        }

        $addr = $1;
        $addr =~ s/ /0/g;
        $addr = "0x$addr";

        $intro = "$addr $func [$file]:";
        my $padlen = 56 - length($intro);
        while ($padlen > 0) {
            $intro .= '	';
            $padlen -= 8;
        }

        $total_size = 0;
    } elsif ($line =~ m/(.*):\s*file format/) {
        $file = $1;
        $file =~ s/\.ko//;
        $lastslash = rindex($file, "/");
        if ($lastslash != -1) {
            $file = substr($file, $lastslash + 1);
        }
    } elsif ($line =~ m/$re/) {
        my $size = $1;
        $size = hex($size) if ($size =~ /^0x/);

        if ($size > 0xf0000000) {
            $size = - $size;
            $size += 0x80000000;
            $size += 0x80000000;
        }
        next if ($size > 0x10000000);

        $total_size += $size;
    } elsif (defined $dre && $line =~ m/$dre/) {
        my $size = $1;

        $size = hex($size) if ($size =~ /^0x/);
        $total_size += $size;
    } elsif (defined $sub) {
        my $size = &$sub($line);

        $total_size += $size;
    }
}

if ($total_size > 0) {
    push @stack, "$intro$total_size\n";
}

# Sort output by size (last field)
print sort { ($b =~ /:\t*(\d+)$/)[0] <=> ($a =~ /:\t*(\d+)$/)[0] } @stack;
