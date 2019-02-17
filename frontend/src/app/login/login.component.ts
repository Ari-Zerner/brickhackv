import { Component, OnInit, Input } from '@angular/core';
import { Router } from '@angular/router';
import { LoginAttempt } from '../models/login-attempt';
import { User } from '../models/user';
import { UserService } from '../services/user.service';

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.scss']
})
export class LoginComponent implements OnInit {

	@Input() username:string = "";
	@Input() password:string = "";

	constructor(private userService:UserService
		,private router:Router) { }

	ngOnInit() {
		
	}

	attemptLogin(){
		let attempt = new LoginAttempt(this.username,this.password)
		console.log("Attempting to Login"+this.username+","+this.password);
		this.userService.attemptLogin(attempt).subscribe((user:User)=>{
			if(user.authenticated){
				this.userService.currentUser = user;
				this.router.navigateByUrl("/dashboard");
			}else{
				// TODO message: login failed
				console.log("Login failed");
			}
		});
	}
}
