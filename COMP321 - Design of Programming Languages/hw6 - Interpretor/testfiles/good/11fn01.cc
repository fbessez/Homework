int doThings(int thingone, int thingtwo) {
   return thingone * thingtwo * 3;
}

int main() {
   int y, z;
   y = 10;
   z = 100;
   printInt(doThings(y, z));

   return 0;
}
