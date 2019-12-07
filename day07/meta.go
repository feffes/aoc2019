package main

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"os/exec"
	"regexp"
	"strconv"
	"sync"
)

var regexint = `(?m)^\d+$`

func main() {
	var wg sync.WaitGroup
	perms := getPermutations([]int{0, 1, 2, 3, 4})
	queue := make(chan int, len(perms))
	fmt.Println("Starting processes")
	for _, md := range perms {
		wg.Add(1)
		go func(md []int) {
			queue <- pajadPiper(md)
		}(md)
	}
	highest := 0
	go func() {
		for t := range queue {
			if t > highest {
				highest = t
			}
			wg.Done()
		}
	}()
	wg.Wait()
	fmt.Printf("Highest: %d\n", highest)
}

// takes a slice of mds in order of execution and returns the result
func pajadPiper(mds []int) int {
	i := 0
	fmt.Printf("Running: %v\n",mds )
	for _, md := range mds {
		i = command(md, i)
	}
	fmt.Printf("Done:%v   Value:%d\n",mds,i)
	return i

}

func command(mode int, input int) int {
	ex := exec.Command("./day07")
	var stdBuffer bytes.Buffer
	ex.Stdout = &stdBuffer
	ex.Stderr = &stdBuffer
	stdin, _ := ex.StdinPipe()
	defer stdin.Close()
	if err := ex.Start(); err != nil {
		log.Panic(err)
	}
	io.WriteString(stdin, strconv.Itoa(mode)+"\n")
	io.WriteString(stdin, strconv.Itoa(input)+"\n")
	ex.Wait()
	save := stdBuffer.String()
	reg, err := regexp.Compile(regexint)
	if err != nil {
		log.Fatalf(err.Error())
	}
	handled := reg.FindString(save)
	out, err := strconv.Atoi(handled)
	if err != nil {
		log.Fatalf("failed to parse output: %s\n err: %s", stdBuffer.String(), err.Error())
	}
	return out
}

func getPermutations(elements []int) [][]int {
	permutations := [][]int{}
	if len(elements) == 1 {
		permutations = [][]int{elements}
		return permutations
	}
	for i := range elements {
		el := make([]int, len(elements))
		copy(el, elements)

		for _, perm := range getPermutations(append(el[0:i], el[i+1:]...)) {
			permutations = append(permutations, append([]int{elements[i]}, perm...))
		}
	}
	return permutations
}
