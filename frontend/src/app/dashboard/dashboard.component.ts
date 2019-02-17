import { Component, OnInit } from '@angular/core';
import { Debate } from '../models/debate';
import { UserService } from '../services/user.service';

@Component({
  selector: 'app-dashboard',
  templateUrl: './dashboard.component.html',
  styleUrls: ['./dashboard.component.scss']
})
export class DashboardComponent implements OnInit {

	debateList:Debate[];

	constructor(private userService:UserService) { }

	ngOnInit() {
		this.userService.getAllDebates().subscribe((_)=>{
			this.debateList = _;
		});
	}

}
