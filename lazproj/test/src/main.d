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
import foo;
import bar;

void main(string args[])
{
    auto ffoo = new Foo;
    auto bbar = new Bar;

    scope(exit)
    {
        delete ffoo;
        delete bbar;
    }
    readln;
}
