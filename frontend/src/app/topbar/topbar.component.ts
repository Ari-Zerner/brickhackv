import { Component, OnInit, Input } from '@angular/core';
import { Location } from '@angular/common';
import { UserService } from '../services/user.service';

@Component({
  selector: 'app-topbar',
  templateUrl: './topbar.component.html',
  styleUrls: ['./topbar.component.scss']
})
export class TopbarComponent implements OnInit {

	@Input() searchTerm = "";

	constructor(private userService:UserService,private location:Location) { }

	ngOnInit() {

	}

	search(){
		console.log("Search for term " + this.searchTerm);
	}

	logout(){
		this.userService.logout().subscribe((success)=>{
			console.log("logout resut received");
			if(success){
				this.userService.currentUser = null;
				this.location.go("");
			}else{
				// TODO messsae : lougout failed
				console.log("Logout failed");
			}
		});
	}

}
