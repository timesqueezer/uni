/* Main.ino file generated by New Project wizard
 *
 * Created:   Wed Mar 24 2021
 * Processor: Arduino Mega
 * Compiler:  Arduino AVR
 */

// Peripheral Configuration Code (do not edit)
//---CONFIG_BEGIN---
#pragma GCC push_options
#pragma GCC optimize ("Os")

#include <core.h> // Required by cpu
#include <cpu.h>

#pragma GCC pop_options

// Peripheral Constructors
CPU &cpu = Cpu;

void peripheral_setup () {
}

void peripheral_loop() {
}
//---CONFIG_END---

// ##### Aufgabe 3.2-3 #####
// timer controlled debounce-routine
// MEGA2650: use of TimerThree or TimerFour or TimerFive library
// detecting of double clicks

void setup() {
  Serial.begin(9600);
  Serial.println("Test");
}

void loop() {
}