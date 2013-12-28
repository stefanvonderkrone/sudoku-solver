var _ = require("lodash"),
    puzzle0, puzzle1, puzzleHard0;

( function() {
    "use strict";

    function replicate(v,n) {
        return _.map(new Array(n), function() {
            return v;
        });
    }

    function solveSudoku( puzzle, base, blockSize ) {

        // console.log("### solveSudoku ###\n");
        // console.log( printSudoku(puzzle, blockSize*blockSize) );
        // console.log("### ########### ###\n");

        function sortPossibles( possibles ) {
            return possibles.sort( function(a,b) {
                return a.possibles.length - b.possibles.length;
            } );
        }

        function getSolution( unkown, puzzle, range ) {

            // console.log("### getSolution ###");
            console.log( unkown.length );

            // console.log("### getSolution ###", (valids.length + remaining.length));
            // console.log("current puzzle:", valids.concat(replicate("x", remaining.length)).join(""));

            function testValues(p, possibles, remaining) {

                function updateRemaining(x) {
                    var size = blockSize * blockSize,
                        indizes = getRow(range, p.at, size)
                            .concat( getColumn( range, p.at, size ) )
                            .concat( getBlock( range, p.at, blockSize ) );
                    return sortPossibles(
                        _.map( remaining, function(p) {
                            return indizes.indexOf(p.at) < 0 ? p : _.extend(p, {
                                possibles: _.difference( p.possibles, x )
                            });
                        } )
                    );
                }

                // console.log(possibles);
                if ( !possibles || possibles.length === 0 ) {
                    // console.log("NO SOLUTION");
                    return undefined;
                } else {
                    var x = possibles[0],
                        xs = possibles.slice(1),
                        solution = getSolution(
                            // remaining,
                            updateRemaining([x]),
                            puzzle.slice( 0, p.at )
                                .concat( [ x ] )
                                .concat( puzzle.slice( p.at + 1 ) ),
                            range
                        );
                    // console.log(x, "is Solution at", valids.length, ":", !!solution);
                    if ( solution !== undefined ) {
                        return solution
                    } else {
                        return testValues(p, xs, remaining);
                    }
                }
            }

            if ( puzzle === undefined ) {
                return undefined;
            } else if ( !unkown || unkown.length === 0 ) {
                return puzzle;
            } else {
                var p = unkown[0],
                    ps = unkown.slice(1);
                return testValues(
                            p,
                            p.possibles,
                            // getTestValues( puzzle, base, p.at, blockSize ),
                            ps
                        );
            }
        }

        var possibles = [];

        _.each( puzzle, function(v,i) {
            if ( v === undefined ) {
                possibles.push( {
                    at: i,
                    possibles: getTestValues(
                        puzzle,
                        base,
                        i,
                        blockSize
                    )
                } )
            }
        } );

        console.log(sortPossibles(possibles));

        return getSolution(
            sortPossibles( possibles ),
            puzzle,
            _.range( puzzle.length )
        );
    }

    function getTestValues( s, base, i, blockSize ) {
        var rowSize = blockSize * blockSize,
            combined = _.union( 
                getRow( s, i, rowSize ),
                    _.union(
                        getColumn( s, i, rowSize ),
                        getBlock( s, i, blockSize ) ) );
        return _.difference( base, combined );
    }

    function parsePuzzle( puzzle, empty ) {
        return _.map( puzzle, function(p) {
            return p === empty ? undefined : p;
        } );
    }

    function getRow( puzzle, index, size ) {
        return _.take( _.drop( puzzle, size * (Math.floor( index / size ) ) ), size );
    }

    function getColumn( puzzle, index, size ) {        

        function buildColumn( index ) {
            return index >= puzzle.length ?
                [] :
                [ puzzle[ index ] ].concat( buildColumn( index + size ) );
        }

        return index >= size ?
            getColumn( puzzle, index - size, size ) :
            buildColumn( index );
    }

    function getBlock( puzzle, index, blockSize ) {
        var rowSize = blockSize * blockSize,
            y = Math.floor( index / (rowSize * blockSize) ),
            x = Math.floor( index / blockSize ) % blockSize;

        function buildBlock( index ) {
            return index < 0 ?
                [] :
                buildBlock( index - 1 ).concat(
                    _.take(
                        _.drop(
                            getRow( puzzle, y * rowSize * blockSize + index * rowSize, rowSize ),
                            x * blockSize ),
                        blockSize
                    ) );
        }

        return buildBlock( blockSize - 1 );
    }

    function printSudoku( puzzle, rowSize ) {
        return puzzle.length > 0 ?
            _.map(puzzle.slice(0,rowSize), function(v) {
                return v === undefined ? "x" : v;
            }).join("") + "\n" +
                printSudoku( puzzle.slice(rowSize), rowSize ) :
            "";
    }

    puzzle0 =
        "------8-7" +
        "3-8-42-65" +
        "---1--932" +
        "-6-75--9-" +
        "95--3--21" +
        "---4-9-5-" +
        "417--3---" +
        "59-814--3" +
        "------4--";

    puzzleHard0 =
        "32-------" +
        "---7-5---" +
        "-------6-" +
        "5-1-----7" +
        "----8--4-" +
        "---6-----" +
        "-8--3-7--" +
        "------5-1" +
        "------9--";

        // "129365847" + //js
        // "129365847" + //js2
        // "129365847" + // haskell
        // "378942165" + //js
        // "378942165" + //js2
        // "378942165" + // haskell
        // "645187932" + //js
        // "645187932" + //js2
        // "645187932" + // haskell
        // "862751394" + //js
        // "862751394" + //js2
        // "862751394" + // haskell
        // "954638721" + //js
        // "954638721" + //js2
        // "954638721" + // haskell
        // "731429658" + //js
        // "731429658" + //js2
        // "731429658" + // haskell
        // "417293586" + //js
        // "417293586" + //js2
        // "417293586" + // haskell
        // "596814273" + //js
        // "596814273" + //js2
        // "596814273" + // haskell
        // "283576419"   //js
        // "283576419"   //js2
        // "283576419"   // haskell



    puzzle1 =[
        0,0,0,0,0,0,8,0,7,
        3,0,8,0,4,2,0,6,5,
        0,0,0,1,0,0,9,3,2,
        0,6,0,7,5,0,0,9,0,
        9,5,0,0,3,0,0,2,1,
        0,0,0,4,0,9,0,5,0,
        4,1,7,0,0,3,0,0,0,
        5,9,0,8,1,4,0,0,3,
        0,0,0,0,0,0,4,0,0
    ];

    console.time("s");
    // _.times(1000, function() {
        console.log(
            printSudoku( solveSudoku(
                parsePuzzle( puzzle0, "-" ),
                "123456789",
                3
            ) || [], 9 )
        );
    // });
    console.timeEnd("s");
}());
