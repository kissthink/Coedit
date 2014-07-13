/*
Test:
- prj save/load
- prj compile/run
- multiples sources
- relative paths
- various switches
*/

module main;

import std.stdio;
import fooclass;
import barclass;

void main(string args[])
{
    auto foo = new Foo;
    auto bar = new Bar;

    scope(exit)
    {
        delete foo;
        delete bar;
    }
    // if not UsePipes in RunOptions
    // then:
    // readln;
    // (input is passed thru the new console)

    // else: input is not handled so readln will hang Coedit until
    // the new process is manually killed
}
