// SKETCH_NAME: au2_template.ino
// ##### Aufgabe 2.x #####
// analog input/output

#define pin_led 13	//externe LED
#define pin_bu  3	//button pin
#define pin_poti 9	//analog input poti

char ch_buffer[80] = "";
float pi_fl = 3.14159265359;

int analog_value_previous = 0;
int analog_value = 0;
float volt_value = 0.f;
float mvolt_value = 0.f;
int pwm_value = 0;

void ftoa(float num, char* res, int numDecPlaces)
{
  dtostrf(num, 4, numDecPlaces, res);
}

void print_float(float num, int numDecPlaces) {
  Serial.print(num, numDecPlaces);
}

void setup()
{
  pinMode(pin_bu, INPUT);
  pinMode(pin_led, OUTPUT);
  analogWrite(pin_led, 255);

  // Initialisierung der ersten seriellen UART -Schnittstelle (9600 Baud)
  Serial.begin(9600);
  Serial.println("Test print Pi\n");
  Serial.print("print Pi using print_float: ");
  print_float(pi_fl , 3);
  Serial.println(" ");
  Serial.print("print Pi using ftoa: ");
  ftoa(pi_fl , ch_buffer , 5);
  Serial.println(ch_buffer);
  
  Serial.print("\ncross check: Serial.println(pi_ch , 5):\t");
  Serial.println(pi_fl , 5);

  pinMode(pin_poti, OUTPUT);
}

void loop()
{
  analog_value = analogRead(pin_poti);
  if (analog_value != analog_value_previous) {
    Serial.print("Analog value changed to: ");
    Serial.println(analog_value);

    mvolt_value = (float) analog_value / 1024.f * 5000.f;
    volt_value = mvolt_value / 1000.f;
    Serial.print("mV: ");
    Serial.print(mvolt_value);
    Serial.print(" | V: ");
    Serial.println(volt_value);

    analog_value_previous = analog_value;

    pwm_value = (int) ( (float) analog_value / 1024.f * 255.f );
    Serial.print("pwm_value: ");
    Serial.println(pwm_value);
    analogWrite(pin_led, pwm_value);
  }
  
}
