# plist-export

plist-export is a simple library to export any combination of Clojure data structures in Apple's [Property list](http://en.wikipedia.org/wiki/Property_list) format. It's fully lazy and should cope fairly well with large datasets (though it's currently not quite memory-flat across infinite sequences).

## Usage

In your project.clj :dependencies:

    [org.clojars.t8r/plist-export "0.0.2"]

There is only one function, which is lazy-plist. Example:

    (use 'plist-export)
    (lazy-plist {:foo "bar" :bar [1 "I'm the number two!" '3 '(4)]})

This will produce a lazy sequence of the following lines:

    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
    <key>:foo</key>
    <string>bar</string>
    <key>:bar</key>
    <array>
    <integer>1</integer>
    <string>I'm the number two!</string>
    <integer>3</integer>
    <array>
    <integer>4</integer>
    </array>
    </array>
    </dict>
    </plist>

## License

Copyright (C) 2012 Adam Sindelar

Distributed under the Eclipse Public License, the same as Clojure.
