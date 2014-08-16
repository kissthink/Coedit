module runnable;

import std.stdio;
import std.getopt;

// pass --a=<CPF> --b=<CPP> --c=<CFF> --d=<CFP> --e=<CI> --f=<CAF> --g=<CAP> as parameters in "Run file..."
void main(string args[])
{

    string expanded;
    for (char c = 'a'; c != 'z'; c++)
    {
        expanded = "";
        getopt(args, std.getopt.config.passThrough, c, &expanded);
        if (expanded.length != 0)
            writeln(expanded);
    }

}
