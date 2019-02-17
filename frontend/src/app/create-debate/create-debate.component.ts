import { Component, OnInit, Input } from '@angular/core';
import { Router } from '@angular/router';
import { Debate } from '../models/debate';
import { UserService } from '../services/user.service';
import { DebateForm } from '../models/debate-form';

@Component({
  selector: 'app-create-debate',
  templateUrl: './create-debate.component.html',
  styleUrls: ['./create-debate.component.scss']
})
export class CreateDebateComponent implements OnInit {

	@Input() debateForm = new DebateForm();

	constructor(private userService:UserService,private router:Router) { }

	ngOnInit() {
	}

	createDebate(){
		this.userService.createDebate(this.debateForm).subscribe((_)=>{
			if(_.id!=null){
				this.router.navigateByUrl("/debate/"+_.id);
			}else{
				// TODO message: Debate creation Failed
				console.log("Debate creation Failed");
			}
		});
	}

	backToDashboard(){
		this.router.navigateByUrl("/dashboard");
	}

	
}
