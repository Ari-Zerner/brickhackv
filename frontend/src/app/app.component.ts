import { Component } from '@angular/core';
import { UserService } from './services/user.service';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.scss']
})
export class AppComponent {
	title = 'frontend';

	constructor(private userService:UserService){	}

	ngOnInit(){
		console.log("hey app component initialized");
		this.userService.testSimpleEndpoint().subscribe((_)=>{ console.log("value from server: "+_.name)})
	}

}
