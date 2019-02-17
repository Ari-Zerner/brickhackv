import { Component, OnInit, Input } from '@angular/core';
import { Router } from '@angular/router';
import { Debate } from '../models/debate';
import { UserService } from '../services/user.service';

@Component({
  selector: 'app-create-debate',
  templateUrl: './create-debate.component.html',
  styleUrls: ['./create-debate.component.scss']
})
export class CreateDebateComponent implements OnInit {

	constructor(private userService:UserService,private router:Router) { }

	ngOnInit() {
	}

	
}
