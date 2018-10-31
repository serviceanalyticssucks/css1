class devicemotion {
  constructor(dataset) {
    this.dataset = dataset
    window.addEventListener('devicemotion', (...args) => this.devicemotionListener(...args))
  }

  //save events from devicemotion into dataset.acceleration
  devicemotionListener(event) {
    // check if(this.dataset.acceleration) {return this.dataset.acceleration;} else {return [];}
    (this.dataset.acceleration = this.dataset.acceleration || []).push(event);
  }

  //retrieve collected features
  features() {
    var f = {}
    if ((this.dataset.acceleration = this.dataset.acceleration || []).length > 0) {
      var a = this.dataset.acceleration
      var acc_x = a.map(v => v.acceleration.x * 1000)
      var acc_y = a.map(v => v.acceleration.y * 1000)
      var acc_z = a.map(v => v.acceleration.z * 1000)

      if (!isArrayValid(acc_x) || !isArrayValid(acc_y) || !isArrayValid(acc_z)) {
        return f;
      }


      f["sdZ"] = math.std(acc_z);
      f["x"] = math.mean(acc_x);
      f["y"] = math.mean(acc_y);
      f["z"] = math.mean(acc_z);
    }
    return f;
  }

  flush() {
    this.dataset.acceleration = [];
  }
}
