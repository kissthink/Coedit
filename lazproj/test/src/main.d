/*
Test:
- prj save/load
- prj compile
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
}
