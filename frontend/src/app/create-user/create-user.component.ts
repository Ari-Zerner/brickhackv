import { Component, OnInit, Input } from '@angular/core';
import { Router } from '@angular/router';
import { Signup } from '../models/signup';
import { UserService } from '../services/user.service';


@Component({
  selector: 'app-create-user',
  templateUrl: './create-user.component.html',
  styleUrls: ['./create-user.component.scss']
})
export class CreateUserComponent implements OnInit {

	@Input() signup = new Signup();
	@Input() confirmPassword = "";

	constructor(private userService:UserService,private router:Router) { }

	ngOnInit() {
	}

	register(){
		if(this.confirmPassword===this.signup.password){
			this.userService.signup(this.signup).subscribe((_)=>{
				// TOOO message : User created successfully
				console.log("User created successfully");
				this.router.navigateByUrl("");
			});
		}else{
			// TOOO message : passwords don't match
			console.log("Passwords don't match");
		}
	}

	backToLogin(){
		this.router.navigateByUrl("");
	}

}
