import React from 'react';
import { ArrowRight } from '@phosphor-icons/react';

interface ProfileCardProps {
	name: string;
    avatar: JSX.Element;
	link: string;
	description?: string; // Optional description field
}

const ProfileCard: React.FC<ProfileCardProps> = ({ name, avatar, link, description }) => {
	return (
		<div className="relative group flex">
            {avatar}
            <span>{name}</span>
			<div className="absolute left-0 bottom-full w-80 h-30 p-2 pb-4 bg-white border border-gray-300 rounded-lg shadow-lg opacity-0 group-hover:opacity-100 transition-opacity duration-300">
				<div className="flex items-center">
                    {avatar}
                    <span className="mr-2 mt-1 flex">
                        {name}
                        <a href={link} target="_blank" rel="noreferrer" className="mt-1 ml-2 block">
                            <ArrowRight weight="bold"/>
                        </a>
                    </span>
				</div>
				{description && (
					<p className="mt-2 ml-6 text-sm text-gray-600 line-clamp-2">
						{description || 'A Khoj agent'}
					</p>
				)}
			</div>
		</div>
	);
};

export default ProfileCard;
