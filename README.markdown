# proto-cl-harmony - proto-cl-harmony is a prototype to add harmony to a given melody

The proto-cl-harmony is toy program to add harmony to a given melody by very simple rules.

## Screenshot

![image](https://user-images.githubusercontent.com/11023733/66269300-f6c45d00-e881-11e9-8e50-1b8dde59adf1.png)

## Installation & Usage

Download sources under a directory managed by quicklisp.

```sh
$ https://github.com/eshamster/ps-experiment.git
$ https://github.com/eshamster/proto-cl-harmony.git
```

Then, load and start server as the following on REPL.

```lisp
CL-USER> (ql:register-local-projects)
...
CL-USER> (ql:quickload :proto-cl-harmony)
...
CL-USER> (proto-cl-harmony:start :port 5000)
...
```

Then, you can access to it by `http://localhost:5000/`.

## Detail
### Tiny subset of MML (Music Macro Language)

This project adopts tiny subset of MML (Music Macro Language) to write melody.

The detail definition is the following.

```txt
<MML> := <block>...
<block> := <note>|<octave>|<rest>

<note> := <tone>[<tone-changer>...][<note len>]
<tone> := "A"|"B"|"C"|"D"|"E"|"F"|"G"
<tone-changer> := "#"|"+"|"-"

<octave> := <set octave>|<inc octave>
<set octave> := "O"integer
<inc octave> := "<"|">"

<rest> := "R"[<note len>]

<note len> := [integer["."...]] ;; Ex. 4 means quater note
```

### Algorighm
#### Add weight to each harmony

Weight for each harmony is calculated in each measure. The weight is added by the following factors.

- If a tone in a measure is included in the harmony.
    - If the tone is the first one of the measure, more weight is added.
    - If the tone is the base one of the harmony, more weight is added.
- If the harmony is not substitute one.

#### Select harmony

Harmonies are selected by the following steps from first measure to last one.

1. Randomly select a harmony according to their weight for each measure.
2. If the sequence of harmonies includes prohibited progression, return to step 1.
3. Repeat above steps N times (the default N is 3).
4. Select the sequence that has highest some of weight.

### Limitation

The followings should be manually specified.

- Scale
- Beat

The supported harmonies satisfy the following conditions.

- It is a triad.
- It includes only tones in a specified scale.

Other limitations:

- Only one harmony is selected to one measure.

## Author

* eshamster (hamgoostar@gmail.com)

## Copyright

Copyright (c) 2019 eshamster (hamgoostar@gmail.com)

## License

Licensed under the LLGPL License.
