import { Component, OnInit, Input, SimpleChanges } from '@angular/core';
import { UserService } from '../services/user.service';
import { Debate } from '../models/debate';
import { Opinion } from '../models/opinion';

@Component({
  selector: 'app-opinion-voting',
  templateUrl: './opinion-voting.component.html',
  styleUrls: ['./opinion-voting.component.scss']
})
export class OpinionVotingComponent implements OnInit {

	@Input() debate:Debate;
	opinionList:Opinion[];// this will be 2 in length

	constructor(private userService:UserService) { }

	ngOnInit() {
		this.userService.getOpinionPairForVoting(this.debate.id).subscribe((_)=>{
			this.opinionList = _;
		});
	}

	// ngOnChanges(changes:SimpleChanges){
	// 	console.log("Changes happened");
		
	// }

}
